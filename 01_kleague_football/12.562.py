# -*- coding: utf-8 -*-
"""
K-League Final Solution V9.5 (Optimized Hybrid GRU)
- Model: Hybrid Context GRU (GELU + Multi-Sample Dropout)
- Logic: Aggressive MoE Ensemble + Long-Distance Decay (Based on 12.570.py)
"""
import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm
import os, random, gc
import torch
from torch import nn
import torch.optim as optim
from torch.nn.utils.rnn import pad_sequence, pack_padded_sequence, pad_packed_sequence
from torch.utils.data import Dataset, DataLoader
# ============================================================
# 0) PATH & CONFIG
# ============================================================
# 경로 본인 환경에 맞게 수정 필요
TRAIN_PATH       = "C:/Users/ADMIN/Downloads/train.csv"
TEST_META_PATH   = "C:/Users/ADMIN/Downloads/test.csv"
SUB_PATH         = "C:/Users/ADMIN/Downloads/sample_submission.csv"
BASE_PATH        = "C:/Users/ADMIN/Downloads"
BATCH_SIZE = 64
EPOCHS     = 100   
LR         = 1e-3
HIDDEN_DIM = 128
DROPOUT    = 0.25
N_PERIOD   = 2
NUMERIC_DIM = 14
MAX_SEQ_LEN = 21
N_FOLDS    = 5     
SEED       = 42    
N_CLUSTERS = 11
CLUSTER_EMB_DIM = 4
#하나의 fold에서 loss를 다르게 계산하여 3가지 모델을 학습
LOSS_VARIANTS = [
    ('base', (1.0, 1.0)),      
    ('x_focus', (2.0, 1.0)),   
    ('y_focus', (1.0, 2.0))
]

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"
print("Using device:", DEVICE)

# ============================================================
# 1) Seed Function
# ============================================================
def set_seed(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    os.environ["PYTHONHASHSEED"] = str(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.deterministic = True

set_seed(SEED)
# ============================================================
# 2) LOAD DATA
# ============================================================
print(">>> Loading Data...")
df = pd.read_csv(TRAIN_PATH)
df = df.sort_values(["game_episode", "time_seconds"]).reset_index(drop=True)
type2id = {v: i for i, v in enumerate(df["type_name"].fillna("NONE").unique())}
res2id  = {v: i for i, v in enumerate(df["result_name"].fillna("NONE").unique())}
team2id = {v: i for i, v in enumerate(df["team_id"].unique())}
df["type_idx"]  = df["type_name"].fillna("NONE").map(type2id).astype(int)
df["res_idx"]   = df["result_name"].fillna("NONE").map(res2id).astype(int)
df["team_idx"]  = df["team_id"].map(team2id).astype(int)
df["home_flag"] = df["is_home"].astype(int)
# ============================================================
# 3) UTILS
# ============================================================
def safe_map(series, mapping, default=0):
    return series.map(lambda x: mapping[x] if x in mapping else default)
#축구장을 12개 공간으로 나누어서 어디에 위치하는지에 대한 변수
def get_zone(x_raw, y_raw):
    w_zone = 105 / 4; h_zone = 68 / 3
    x_idx = int(x_raw / w_zone); y_idx = int(y_raw / h_zone)
    x_idx = min(max(x_idx, 0), 3); y_idx = min(max(y_idx, 0), 2)
    return y_idx * 4 + x_idx 
#목표 골대 까지의 거리, 각도
def get_goal_info(x_raw, y_raw):
    goal_x, goal_y = 105, 34
    dx = goal_x - x_raw; dy = goal_y - y_raw
    dist = np.sqrt(dx**2 + dy**2)
    angle = np.arctan2(dy, dx)
    return dist / 105.0, np.sin(angle), np.cos(angle)
#추가시간 여부
def is_added_time(time_sec):
    return time_sec > (45 * 60)
#단순 clip 보다는 공이 나가는 경로를 따라서 계산
def correct_out_of_bounds(start_x, start_y, pred_end_x, pred_end_y):
    MIN_X, MAX_X = 0, 105; MIN_Y, MAX_Y = 0, 68
    if (MIN_X <= pred_end_x <= MAX_X) and (MIN_Y <= pred_end_y <= MAX_Y):
        return pred_end_x, pred_end_y
    dx = pred_end_x - start_x; dy = pred_end_y - start_y
    if not ((MIN_X <= start_x <= MAX_X) and (MIN_Y <= start_y <= MAX_Y)):
         return np.clip(pred_end_x, MIN_X, MAX_X), np.clip(pred_end_y, MIN_Y, MAX_Y)
    candidates = []
    if pred_end_x < MIN_X and abs(dx) > 1e-6:
        t = (MIN_X - start_x) / dx
        y_hit = start_y + t * dy
        if MIN_Y <= y_hit <= MAX_Y: candidates.append((MIN_X, y_hit))
    if pred_end_x > MAX_X and abs(dx) > 1e-6:
        t = (MAX_X - start_x) / dx
        y_hit = start_y + t * dy
        if MIN_Y <= y_hit <= MAX_Y: candidates.append((MAX_X, y_hit))
    if pred_end_y < MIN_Y and abs(dy) > 1e-6:
        t = (MIN_Y - start_y) / dy
        x_hit = start_x + t * dx
        if MIN_X <= x_hit <= MAX_X: candidates.append((x_hit, MIN_Y))
    if pred_end_y > MAX_Y and abs(dy) > 1e-6:
        t = (MAX_Y - start_y) / dy
        x_hit = start_x + t * dx
        if MIN_X <= x_hit <= MAX_X: candidates.append((x_hit, MAX_Y))
    if candidates:
        best_cand = candidates[0]
        min_dist = (best_cand[0]-start_x)**2 + (best_cand[1]-start_y)**2
        for cand in candidates[1:]:
            dist = (cand[0]-start_x)**2 + (cand[1]-start_y)**2
            if dist < min_dist:
                min_dist = dist; best_cand = cand
        return best_cand
    return np.clip(pred_end_x, MIN_X, MAX_X), np.clip(pred_end_y, MIN_Y, MAX_Y)
# ============================================================
# 4) Clustering
# ============================================================
#player 행동, 위치 정보를 기반으로 클러스터링
def create_advanced_player_clusters(target_df, n_clusters=11):
    print(f"\n>>> Generating Player Clusters (n={n_clusters})...")
    temp_df = target_df.copy()
    temp_df['norm_start_x'] = temp_df['start_x']
    temp_df['norm_start_y'] = temp_df['start_y']
    temp_df['is_forward'] = (temp_df['end_x'] > temp_df['start_x']).astype(int)
    spatial = temp_df.groupby('player_id').agg({
        'norm_start_x': ['mean', 'std'], 'norm_start_y': ['mean', 'std'], 'is_forward': 'mean'
    })
    spatial.columns = [f"{x}_{y}" for x, y in spatial.columns]
    # Simple Action Counts
    def_events = ['Tackle', 'Interception', 'Clearance', 'Block', 'Aerial Clearance', 'Duel', 'Recovery', 'Intervention', 'Foul']
    att_events = ['Shot', 'Shot_Freekick', 'Cross', 'Take-On', 'Pass_Corner', 'Pass_Freekick', 'Penalty Kick']
    pass_events = ['Pass']; gk_events = ['Goal Kick', 'Catch', 'Parry']
    total_actions = temp_df.groupby('player_id').size()
    games_played  = temp_df.groupby('player_id')['game_id'].nunique()
    def_counts  = temp_df[temp_df['type_name'].isin(def_events)].groupby('player_id').size()
    att_counts  = temp_df[temp_df['type_name'].isin(att_events)].groupby('player_id').size()
    pass_counts = temp_df[temp_df['type_name'].isin(pass_events)].groupby('player_id').size()
    gk_counts   = temp_df[temp_df['type_name'].isin(gk_events)].groupby('player_id').size()
    behavioral = pd.DataFrame(index=total_actions.index)
    behavioral['def_ratio']  = (def_counts / total_actions).fillna(0)
    behavioral['att_ratio']  = (att_counts / total_actions).fillna(0)
    behavioral['pass_ratio'] = (pass_counts / total_actions).fillna(0)
    behavioral['gk_ratio']   = (gk_counts / total_actions).fillna(0)
    behavioral['actions_per_game'] = (total_actions / games_played).fillna(0)
    features = pd.concat([spatial, behavioral], axis=1).fillna(0)
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(features)
    kmeans = KMeans(n_clusters=n_clusters, random_state=42, n_init=30)
    clusters = kmeans.fit_predict(X_scaled)
    features['cluster'] = clusters
    features['sort_score'] = features['norm_start_x_mean'] + (features['att_ratio'] * 50) 
    cluster_order = features.groupby('cluster')['sort_score'].mean().sort_values().index
    mapper = {old_idx: new_idx for new_idx, old_idx in enumerate(cluster_order)}
    features['sorted_cluster'] = features['cluster'].map(mapper)
    
    return dict(zip(features.index, features['sorted_cluster'] + 1))

player2cluster = create_advanced_player_clusters(df, n_clusters=N_CLUSTERS)
df['cluster_idx'] = df['player_id'].map(player2cluster).fillna(0).astype(int)

# ============================================================
# 5) Episode Builder
# ============================================================
# 마지막 행동을 하는 팀을 기준으로 공이 연결되도록 수정
def calculate_seq_features(sx, sy, ex, ey, time_arr, type_idx, anchor_team, team_idx):
    seq_num = []
    sx_raw = sx * 105.0; sy_raw = sy * 68.0
    ex_raw = ex * 105.0; ey_raw = ey * 68.0
    prev_x, prev_y = sx[0], sy[0]; prev_time = time_arr[0]; prev_type = -1
    length = len(sx)
      
    for i in range(length):
        cx, cy = sx[i], sy[i]
        curr_team = team_idx[i]
        is_anchor = 1.0 if curr_team == anchor_team else 0.0
        dx = cx - prev_x; dy = cy - prev_y
        dt_raw = max(time_arr[i] - prev_time, 0.0)
        dt_norm = dt_raw / 10.0
        speed = np.sqrt((dx/dt_raw)**2 + (dy/dt_raw)**2) if dt_raw > 0 else 0.0
        curr_type = type_idx[i]
        is_same_type = 1.0 if curr_type == prev_type else 0.0
        
        g_dist, g_sin, g_cos = get_goal_info(sx_raw[i], sy_raw[i])
        touchline_dist = min(cy, 1.0 - cy) * 2.0
        is_stationary = 1.0 if (abs(dx) < 1e-6 and abs(dy) < 1e-6) else 0.0
        
        seq_num.append([cx, cy, dx, dy, is_anchor, dt_norm, speed, g_dist, g_sin, g_cos, touchline_dist, is_stationary, is_same_type])
        prev_x, prev_y = cx, cy; prev_time = time_arr[i]; prev_type = curr_type
        
        if i < length - 1:
            ex_i, ey_i = ex[i], ey[i]
            dx2, dy2 = ex_i - prev_x, ey_i - prev_y
            g_dist2, g_sin2, g_cos2 = get_goal_info(ex_raw[i], ey_raw[i])
            cy2 = ey_i 
            touchline_dist2 = min(cy2, 1.0 - cy2) * 2.0
            is_stationary2 = 1.0 if (abs(dx2) < 1e-6 and abs(dy2) < 1e-6) else 0.0
            
            seq_num.append([ex_i, ey_i, dx2, dy2, is_anchor, 0.0, 0.0, g_dist2, g_sin2, g_cos2, touchline_dist2, is_stationary2, 1.0])
            prev_x, prev_y = ex_i, ey_i
            
    return np.array(seq_num, dtype="float32")

def build_episodes(df_sub, augment=True):
    ep_num, ep_cat, ep_zone, ep_tgt = [], [], [], [] 

    for _, g in tqdm(df_sub.groupby("game_episode"), desc="Build Eps", leave=False):
        g = g.reset_index(drop=True)
        if len(g) < 2: continue

        sx_raw = g["start_x"].values; sy_raw = g["start_y"].values
        ex_raw = g["end_x"].values; ey_raw = g["end_y"].values
        sx, sy = sx_raw / 105.0, sy_raw / 68.0; ex, ey = ex_raw / 105.0, ey_raw / 68.0
        type_idx = g["type_idx"].values; res_idx = g["res_idx"].values
        team_idx = g["team_idx"].values; home_flag = g["home_flag"].values
        period = np.clip(g["period_id"].values.astype(int), 0, N_PERIOD - 1)
        time_arr = g["time_seconds"].values; c_ids = g["cluster_idx"].values
        
        first_time = time_arr[0]
        is_added_seq = 1 if is_added_time(first_time) else 0
        added = np.full(len(g), is_added_seq, dtype=np.int64)
        
        anchor_team = team_idx[-1]; flip_mask = (team_idx != anchor_team)
        sx[flip_mask] = 1.0 - sx[flip_mask]; sy[flip_mask] = 1.0 - sy[flip_mask]
        ex[flip_mask] = 1.0 - ex[flip_mask]; ey[flip_mask] = 1.0 - ey[flip_mask]
        
        # 6 Scenarios Augmentation
        scenarios = [(False, False, False, False)]
#적절한 증강 추가
        if augment:
            scenarios.append((True, False, False, False)) # Flip
            scenarios.append((False, True, False, False)) # Noise
#            scenarios.append((True, True, False, False))  # Noise+Flip
            scenarios.append((False, False, True, False)) # Rot
            scenarios.append((True, False, True, False))  # Rot+Flip
#            scenarios.append((False, True, True, True)) # TimeScale (New)
            scenarios.append((True, True, True, True))  # TimeScale+Flip (New) 
            
        for do_flip, do_noise, do_rot, do_time_scale in scenarios:
            curr_sx, curr_sy = sx.copy(), sy.copy()
            curr_ex, curr_ey = ex.copy(), ey.copy()
            curr_time = time_arr.copy() 
#상하 반전
            if do_flip:
                curr_sy = 1.0 - curr_sy; curr_ey = 1.0 - curr_ey
#회전             
            if do_rot:
                angle_deg = np.random.uniform(-15, 15)
                theta = np.radians(angle_deg)
                c, s = np.cos(theta), np.sin(theta)
                cx_rot, cy_rot = 0.5, 0.5
                curr_sx_c = curr_sx - cx_rot; curr_sy_c = curr_sy - cy_rot
                curr_sx = (curr_sx_c * c - curr_sy_c * s) + cx_rot
                curr_sy = (curr_sx_c * s + curr_sy_c * c) + cy_rot
                curr_ex_c = curr_ex - cx_rot; curr_ey_c = curr_ey - cy_rot
                curr_ex = (curr_ex_c * c - curr_ey_c * s) + cx_rot
                curr_ey = (curr_ex_c * s + curr_ey_c * c) + cy_rot
                curr_sx = np.clip(curr_sx, 0.0, 1.0); curr_sy = np.clip(curr_sy, 0.0, 1.0)
                curr_ex = np.clip(curr_ex, 0.0, 1.0); curr_ey = np.clip(curr_ey, 0.0, 1.0)
#조금의 노이즈와 쉬프팅
            if do_noise:
                shift_x = np.random.uniform(-0.02, 0.02); shift_y = np.random.uniform(-0.02, 0.02)
                noise_sx = np.random.normal(0, 0.005, size=len(curr_sx))
                noise_sy = np.random.normal(0, 0.005, size=len(curr_sy))
                noise_ex = np.random.normal(0, 0.005, size=len(curr_ex))
                noise_ey = np.random.normal(0, 0.005, size=len(curr_ey))
                
                # Maintain connection
                if len(curr_sx) > 1:
                    dist_connect = (curr_ex[:-1] - curr_sx[1:])**2 + (curr_ey[:-1] - curr_sy[1:])**2
                    is_connected = dist_connect < 1e-9 
                    noise_sx[1:][is_connected] = noise_ex[:-1][is_connected]
                    noise_sy[1:][is_connected] = noise_ey[:-1][is_connected]
                
                # Maintain static
                dist_static = (curr_sx - curr_ex)**2 + (curr_sy - curr_ey)**2
                is_static = dist_static < 1e-9 
                noise_ex[is_static] = noise_sx[is_static]
                noise_ey[is_static] = noise_sy[is_static]
                
                curr_sx = np.clip(curr_sx + shift_x + noise_sx, 0.0, 1.0)
                curr_sy = np.clip(curr_sy + shift_y + noise_sy, 0.0, 1.0)
                curr_ex = np.clip(curr_ex + shift_x + noise_ex, 0.0, 1.0)
                curr_ey = np.clip(curr_ey + shift_y + noise_ey, 0.0, 1.0)
#시간정보 변화            
            if do_time_scale:
                scale_factor = np.random.uniform(0.8, 1.2)
                start_t = curr_time[0]
                curr_time = start_t + (curr_time - start_t) * scale_factor

            seq_num = calculate_seq_features(curr_sx, curr_sy, curr_ex, curr_ey, curr_time, type_idx, anchor_team, team_idx)
            seq_cat, seq_zone = [], []
            for i in range(len(g)):
                z_id = get_zone(curr_sx[i]*105, curr_sy[i]*68)
                seq_cat.append([type_idx[i], res_idx[i], team_idx[i], home_flag[i], period[i], added[i], c_ids[i]])
                seq_zone.append(z_id)
                if i < len(g) - 1:
                    z_id2 = get_zone(curr_ex[i]*105, curr_ey[i]*68)
                    seq_cat.append([type_idx[i], res_idx[i], team_idx[i], home_flag[i], period[i], added[i], c_ids[i]])
                    seq_zone.append(z_id2)
            seq_cat = np.array(seq_cat, dtype="int64"); seq_zone= np.array(seq_zone, dtype="int64")
            
            if len(seq_num) > MAX_SEQ_LEN:
                seq_num = seq_num[-MAX_SEQ_LEN:]; seq_cat = seq_cat[-MAX_SEQ_LEN:]; seq_zone = seq_zone[-MAX_SEQ_LEN:]
            
            T = len(seq_num)
            t_idx = np.arange(T)/(T-1) if T > 1 else np.zeros(T, dtype="float32")
            seq_num = np.hstack([seq_num, t_idx.reshape(-1, 1)])
            
            last_obs_x, last_obs_y = seq_num[-1, 0], seq_num[-1, 1]
            final_target_x = curr_ex[-1]; final_target_y = curr_ey[-1]
            target_delta = np.array([final_target_x - last_obs_x, final_target_y - last_obs_y], dtype="float32")
            
            ep_num.append(seq_num); ep_cat.append(seq_cat); ep_zone.append(seq_zone)
            ep_tgt.append(target_delta)

    return ep_num, ep_cat, ep_zone, ep_tgt

# ============================================================
# 6) Dataset & DataLoader
# ============================================================
class EpisodeDataset(Dataset):
    def __init__(self, data):
        self.ep_num, self.ep_cat, self.ep_zone, self.targets = data
    def __len__(self): return len(self.ep_num)
    def __getitem__(self, idx):
        num  = torch.tensor(self.ep_num[idx],  dtype=torch.float32)
        cat  = torch.tensor(self.ep_cat[idx],  dtype=torch.long)
        zone = torch.tensor(self.ep_zone[idx], dtype=torch.long)
        tgt  = torch.tensor(self.targets[idx], dtype=torch.float32)
        last_pos = num[-1, :2] 
        return num, cat, zone, tgt, num.shape[0], last_pos

def collate_fn(batch):
    xs_num, xs_cat, xs_zone, ys, lengths, last_pos = zip(*batch)
    lengths = torch.tensor(lengths, dtype=torch.long)
    ys = torch.stack(ys)
    last_pos = torch.stack(last_pos)
    pad_num  = pad_sequence(xs_num,  batch_first=True)
    pad_cat  = pad_sequence(xs_cat,  batch_first=True, padding_value=0)
    pad_zone = pad_sequence(xs_zone, batch_first=True, padding_value=0)
    return pad_num, pad_cat, pad_zone, ys, lengths, last_pos

# ============================================================
# 7) Model Components & Loss
# ============================================================
class WeightedAdaptiveHuberLoss(nn.Module):
    def __init__(self, quantile=0.5, weights=(1.0, 1.0)):
        super().__init__()
        self.quantile = quantile
        self.weights = torch.tensor(weights, dtype=torch.float32)
        
    def forward(self, pred, target):
        diff = torch.abs(pred - target) # (B, 2)
        delta = torch.quantile(diff.detach(), self.quantile)
        loss = torch.where(diff < delta, 0.5 * diff ** 2, delta * (diff - 0.5 * delta))
        w = self.weights.to(pred.device) # (2,)
        weighted_loss = loss * w 
        return weighted_loss.mean()
# [NEW] Multi-Sample Dropout Class (V9.5 Key Feature)
class MultiSampleDropout(nn.Module):
    def __init__(self, p=0.5, num_samples=5):
        super().__init__()
        self.p = p
        self.num_samples = num_samples
        self.dropout = nn.Dropout(p)

    def forward(self, x, layer):
        if not self.training:
            return layer(x)
        out = []
        for _ in range(self.num_samples):
            out.append(layer(self.dropout(x)))
        return torch.mean(torch.stack(out), dim=0)
# ============================================================
# [MODEL] Hybrid Context GRU V9.5 (GELU + MSD)
# ============================================================
class GRUTransformerModel(nn.Module):
    def __init__(self, numeric_dim=NUMERIC_DIM, hidden_dim=HIDDEN_DIM):
        super().__init__()
        # 1. Embeddings
        self.emb_type    = nn.Embedding(len(type2id), 8)
        self.emb_res     = nn.Embedding(len(res2id), 4)
        self.emb_team    = nn.Embedding(len(team2id), 8)
        self.emb_period  = nn.Embedding(N_PERIOD, 2)            
        self.emb_zone    = nn.Embedding(12, 4) 
        self.emb_added   = nn.Embedding(2, 2)
        self.emb_cluster = nn.Embedding(N_CLUSTERS + 1, CLUSTER_EMB_DIM) 
        # 2. Time-Aware Encoding (GELU Applied)
        self.dt_encoder = nn.Sequential(
            nn.Linear(1, 16),
            nn.GELU(), # [V9.5 Change]
            nn.Linear(16, 16)
        )
        self.input_dim = numeric_dim + 8 + 4 + 8 + 1 + 4 + CLUSTER_EMB_DIM + 16
        # 3. Backbone (GRU + Transformer)
        self.gru = nn.GRU(
            input_size=self.input_dim, hidden_size=hidden_dim, num_layers=2, 
            batch_first=True, bidirectional=True, dropout=DROPOUT
        )
        self.d_model = hidden_dim * 2 
        # Transformer with GELU
        self.transformer_layer = nn.TransformerEncoderLayer(
            d_model=self.d_model, nhead=4, dim_feedforward=self.d_model * 2, 
            dropout=DROPOUT, batch_first=True, activation='gelu' # [V9.5 Change]
        )
        self.transformer = nn.TransformerEncoder(self.transformer_layer, num_layers=1)
        self.attention_weights = nn.Linear(self.d_model, 1)
        # 4. Gating Mechanism (GELU Applied)
        self.gate_fc = nn.Sequential(
            nn.Linear(self.d_model * 2 + hidden_dim, hidden_dim),
            nn.GELU(), # [V9.5 Change]
            nn.Linear(hidden_dim, self.d_model),
            nn.Sigmoid()
        )
        # 5. Static Layers (GELU Applied)
        self.static_mlp = nn.Sequential(
            nn.Linear(2 + 2, hidden_dim), 
            nn.GELU(), # [V9.5 Change]
            nn.Dropout(DROPOUT)
        )
        # [V9.5 Change] Final Layer Normalization
        self.final_norm = nn.LayerNorm(self.d_model + hidden_dim)
        # [V9.5 Change] Multi-Sample Dropout
        self.msd = MultiSampleDropout(p=DROPOUT, num_samples=5)
        self.fc_out = nn.Linear(hidden_dim, 2) 
        # Pre-FC for MSD input
        self.pre_fc = nn.Sequential(
            nn.Linear(self.d_model + hidden_dim, hidden_dim), 
            nn.GELU()
        )

    def forward(self, num_x, cat_x, zone_x, lengths):
        e_type = self.emb_type(cat_x[:, :, 0]); e_res = self.emb_res(cat_x[:, :, 1])
        e_team = self.emb_team(cat_x[:, :, 2]); e_home = cat_x[:, :, 3].float().unsqueeze(-1)
        e_clust = self.emb_cluster(cat_x[:, :, 6]); e_zone = self.emb_zone(zone_x)
        dt_val = num_x[:, :, 5:6]; e_dt = self.dt_encoder(dt_val)
        x = torch.cat([num_x, e_type, e_res, e_team, e_home, e_zone, e_clust, e_dt], dim=-1)
        # 2. GRU
        packed = pack_padded_sequence(x, lengths.cpu(), batch_first=True, enforce_sorted=False)
        gru_out_packed, _ = self.gru(packed)
        gru_out, _ = pad_packed_sequence(gru_out_packed, batch_first=True) 
        
        B, T_max, _ = gru_out.size()
        pad_mask = torch.arange(T_max, device=gru_out.device).unsqueeze(0) >= lengths.unsqueeze(1)
        
        # 3. Context Extraction
        idx = (lengths - 1).view(-1, 1).expand(B, gru_out.size(2)).unsqueeze(1)
        h_local = gru_out.gather(1, idx).squeeze(1) 
        
        trans_out = self.transformer(gru_out, src_key_padding_mask=pad_mask)
        attn_score = self.attention_weights(trans_out).masked_fill(pad_mask.unsqueeze(-1), -1e9)
        h_global = torch.sum(trans_out * torch.softmax(attn_score, dim=1), dim=1)

        # 4. Static
        e_period = self.emb_period(cat_x[:, 0, 4])
        e_added = self.emb_added(cat_x[:, 0, 5])
        h_static = self.static_mlp(torch.cat([e_period, e_added], dim=-1))
        
        # 5. Gating
        gate_input = torch.cat([h_local, h_global, h_static], dim=-1)
        gate = self.gate_fc(gate_input) 
        h_fused = gate * h_local + (1 - gate) * h_global
        
        # 6. Final Prediction with MSD
        final_input = torch.cat([h_fused, h_static], dim=-1)
        final_input = self.final_norm(final_input) # 안정성 확보
        
        feat = self.pre_fc(final_input)
        
        # Multi-Sample Dropout Execution
        return self.msd(feat, self.fc_out)
#valid, test 에서 TTA y축 반전으로 예측2개를 평균
def predict_tta(model, num_x, cat_x, zone_x, lengths): 
    pred_orig = model(num_x, cat_x, zone_x, lengths)
    # Flip TTA
    num_flip = num_x.clone()
    num_flip[:, :, 1] = 1.0 - num_flip[:, :, 1]
    num_flip[:, :, 3] = -num_flip[:, :, 3]; num_flip[:, :, 8] = -num_flip[:, :, 8]
    zone_map = torch.tensor([8, 9, 10, 11, 4, 5, 6, 7, 0, 1, 2, 3], device=zone_x.device)
    zone_flip = zone_map[zone_x]
    
    pred_flip = model(num_flip, cat_x, zone_flip, lengths)
    pred_flip[:, 1] = -pred_flip[:, 1] 
    
    return (pred_orig + pred_flip) / 2.0

# ============================================================
# 8) 5-FOLD TRAINING LOOP (3 Variants per Fold)
# ============================================================
unique_games = df["game_id"].unique()
kf = KFold(n_splits=N_FOLDS, shuffle=True, random_state=SEED)

print(f"\n>>> Start 5-Fold Training with HybridContextGRU V9.5 (GELU+MSD)...")

for fold, (train_idx, val_idx) in enumerate(kf.split(unique_games)):
    print(f"\n=== Fold {fold+1}/{N_FOLDS} ===")
    train_gids = unique_games[train_idx]
    valid_gids = unique_games[val_idx]
    
    train_df = df[df["game_id"].isin(train_gids)].reset_index(drop=True)
    valid_df = df[df["game_id"].isin(valid_gids)].reset_index(drop=True)
    
    train_data = build_episodes(train_df, augment=True)
    valid_data = build_episodes(valid_df, augment=False)
    
    train_loader = DataLoader(EpisodeDataset(train_data), batch_size=BATCH_SIZE, shuffle=True, collate_fn=collate_fn)
    valid_loader = DataLoader(EpisodeDataset(valid_data), batch_size=BATCH_SIZE, shuffle=False, collate_fn=collate_fn)
    
    for variant_name, variant_weights in LOSS_VARIANTS:
        print(f"  >> Training [{variant_name}] (Weights: {variant_weights})...")
        
        model = GRUTransformerModel(numeric_dim=NUMERIC_DIM).to(DEVICE)
        optimizer = optim.Adam(model.parameters(), lr=LR, weight_decay=1e-5)
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='min', factor=0.5, patience=5)
        
        criterion = WeightedAdaptiveHuberLoss(quantile=0.5, weights=variant_weights).to(DEVICE)
        
        best_dist = float("inf")
        best_state = None
        save_path = f"./best_model_fold_{fold}_{variant_name}.pth"
        
        for epoch in range(1, EPOCHS + 1):
            model.train()
            tr_losses = []
            for num_x, cat_x, zone_x, delta_y, lengths, _ in train_loader:
                num_x, cat_x, zone_x = num_x.to(DEVICE), cat_x.to(DEVICE), zone_x.to(DEVICE)
                delta_y = delta_y.to(DEVICE)
                lengths = lengths.to(DEVICE)
                
                optimizer.zero_grad()
                pred = model(num_x, cat_x, zone_x, lengths)
                loss = criterion(pred, delta_y)
                loss.backward()
                optimizer.step()
                tr_losses.append(loss.item())
            
            train_loss = np.mean(tr_losses)
            model.eval()
            vlist = []
            with torch.no_grad():
                for num_x, cat_x, zone_x, delta_y, lengths, last_pos in valid_loader:
                    num_x, cat_x, zone_x = num_x.to(DEVICE), cat_x.to(DEVICE), zone_x.to(DEVICE)
                    lengths = lengths.to(DEVICE)
                    
                    pred_delta = predict_tta(model, num_x, cat_x, zone_x, lengths).cpu()
                    
                    batch_dist = []
                    for i in range(len(last_pos)):
                        lx, ly = last_pos[i, 0].item() * 105, last_pos[i, 1].item() * 68
                        px_end_raw = lx + (pred_delta[i, 0].item() * 105)
                        py_end_raw = ly + (pred_delta[i, 1].item() * 68)
                        
                        px_corrected, py_corrected = correct_out_of_bounds(lx, ly, px_end_raw, py_end_raw)
                        
                        tx = lx + (delta_y[i, 0].item() * 105)
                        ty = ly + (delta_y[i, 1].item() * 68)
                        
                        d = np.sqrt((px_corrected - tx)**2 + (py_corrected - ty)**2)
                        batch_dist.append(d)
                    vlist.extend(batch_dist)
            
            valid_dist = np.mean(vlist)
            scheduler.step(valid_dist)
            
            if epoch % 10 == 0 or valid_dist < best_dist:
                 print(f"    [Ep {epoch}] {variant_name} | ValDist: {valid_dist:.4f}")
            
            if valid_dist < best_dist:
                best_dist = valid_dist
                best_state = model.state_dict()
        
        if best_state is not None:
            torch.save(best_state, save_path)
            print(f"    Saved {save_path} (Best ValDist: {best_dist:.4f})")
        
        del model, optimizer, criterion
        torch.cuda.empty_cache()

    del train_loader, valid_loader, train_data, valid_data
    gc.collect()

# ============================================================
# 9) INFERENCE
# ============================================================
print("\n>>> Start Inference V9.5 (Aggressive MoE + Decay)...")

model_groups = {'base': [], 'x_focus': [], 'y_focus': []}

for fold in range(N_FOLDS):
    for variant_name, _ in LOSS_VARIANTS:
        path = f"./best_model_fold_{fold}_{variant_name}.pth"
        if os.path.exists(path):
            m = GRUTransformerModel(numeric_dim=NUMERIC_DIM).to(DEVICE)
            m.load_state_dict(torch.load(path, map_location=DEVICE))
            m.eval()
            model_groups[variant_name].append(m)
        else:
            print(f"Warning: Model not found {path}")

print(f"Loaded Models for V9.5")
W_FOR_X = {'base': 0.3, 'x_focus': 0.6, 'y_focus': 0.1}
W_FOR_Y = {'base': 0.3, 'x_focus': 0.1, 'y_focus': 0.6}
submission = pd.read_csv(SUB_PATH).merge(pd.read_csv(TEST_META_PATH), on="game_episode", how="left")
file_paths = [row["path"].replace("./", f"{BASE_PATH}/") for _, row in submission.iterrows()]
preds_x, preds_y = [], []

with torch.no_grad():
    for fpath in tqdm(file_paths, total=len(file_paths), desc="Inference V9.5"):
        g = pd.read_csv(fpath).reset_index(drop=True)
        sx_raw, sy_raw = g["start_x"].values, g["start_y"].values
        ex_raw, ey_raw = g["end_x"].values, g["end_y"].values
        sx, sy = sx_raw / 105.0, sy_raw / 68.0; ex, ey = ex_raw / 105.0, ey_raw / 68.0
        type_idx = safe_map(g["type_name"].fillna("NONE"), type2id).astype(int).values
        res_idx  = safe_map(g["result_name"].fillna("NONE"), res2id).astype(int).values
        team_idx = safe_map(g["team_id"], team2id).astype(int).values
        home_flag= g["is_home"].astype(int).values
        period   = np.clip(g["period_id"].values.astype(int), 0, N_PERIOD - 1)
        time_arr = g["time_seconds"].values
        curr_pids = g["player_id"].values
        c_ids = np.array([player2cluster.get(pid, 0) for pid in curr_pids], dtype=int)
        first_time = time_arr[0]
        is_added_seq = 1 if is_added_time(first_time) else 0
        added = np.full(len(g), is_added_seq, dtype=np.int64)
        anchor_team = team_idx[-1]; flip_mask = (team_idx != anchor_team)
        
        sx[flip_mask] = 1.0 - sx[flip_mask]; sy[flip_mask] = 1.0 - sy[flip_mask]
        ex[flip_mask] = 1.0 - ex[flip_mask]; ey[flip_mask] = 1.0 - ey[flip_mask]
        
        seq_num = calculate_seq_features(sx, sy, ex, ey, time_arr, type_idx, anchor_team, team_idx)
        seq_cat, seq_zone = [], []
        for i in range(len(g)):
            z_id = get_zone(sx[i]*105, sy[i]*68)
            seq_cat.append([type_idx[i], res_idx[i], team_idx[i], home_flag[i], period[i], added[i], c_ids[i]])
            seq_zone.append(z_id)
            if i < len(g) - 1:
                z_id2 = get_zone(ex[i]*105, ey[i]*68)
                seq_cat.append([type_idx[i], res_idx[i], team_idx[i], home_flag[i], period[i], added[i], c_ids[i]])
                seq_zone.append(z_id2)
        
        seq_cat = np.array(seq_cat, dtype="int64"); seq_zone= np.array(seq_zone, dtype="int64")
        if len(seq_num) > MAX_SEQ_LEN:
            seq_num = seq_num[-MAX_SEQ_LEN:]; seq_cat = seq_cat[-MAX_SEQ_LEN:]; seq_zone = seq_zone[-MAX_SEQ_LEN:]

        T = len(seq_num)
        t_idx = np.arange(T)/(T-1) if T > 1 else np.zeros(T, dtype="float32")
        seq_num = np.hstack([seq_num, t_idx.reshape(-1, 1)])      
        nx = torch.tensor(seq_num, dtype=torch.float32).unsqueeze(0).to(DEVICE)
        cx_t = torch.tensor(seq_cat, dtype=torch.long).unsqueeze(0).to(DEVICE)
        zx_t = torch.tensor(seq_zone, dtype=torch.long).unsqueeze(0).to(DEVICE)
        ln = torch.tensor([T], dtype=torch.long).to(DEVICE)
        group_preds = {}
        for v_name, models in model_groups.items():
            if len(models) == 0:
                group_preds[v_name] = np.zeros(2)
                continue
                
            fold_preds = []
            for m in models:
                p = predict_tta(m, nx, cx_t, zx_t, ln).cpu().numpy()[0]
                fold_preds.append(p)
            group_preds[v_name] = np.mean(fold_preds, axis=0)

        dx = (group_preds['base'][0]*W_FOR_X['base'] + group_preds['x_focus'][0]*W_FOR_X['x_focus'] + group_preds['y_focus'][0]*W_FOR_X['y_focus'])
        dy = (group_preds['base'][1]*W_FOR_Y['base'] + group_preds['x_focus'][1]*W_FOR_Y['x_focus'] + group_preds['y_focus'][1]*W_FOR_Y['y_focus'])
        
        dist_norm = np.sqrt(dx**2 + dy**2)
        if dist_norm > 0.28: 
            dx *= 0.99
            dy *= 0.99
        
        last_x, last_y = seq_num[-1, 0] * 105, seq_num[-1, 1] * 68
        pred_x_raw = last_x + (dx * 105)
        pred_y_raw = last_y + (dy * 68)
        
        final_x, final_y = correct_out_of_bounds(last_x, last_y, pred_x_raw, pred_y_raw)
        preds_x.append(final_x)
        preds_y.append(final_y)

submission["end_x"] = preds_x
submission["end_y"] = preds_y
save_name = "./final_submission_v9_5_Optimized.csv"
submission[["game_episode", "end_x", "end_y"]].to_csv(save_name, index=False)
print(f"Done. Saved to {save_name}")