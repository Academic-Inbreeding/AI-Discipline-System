import pandas as pd
from sklearn.preprocessing import StandardScaler
from statsmodels.tsa.stattools import grangercausalitytests
from sklearn.preprocessing import StandardScaler
from statsmodels.tsa.stattools import grangercausalitytests
from collections import defaultdict


df1=pd.read_csv(r'C:\Users\80577\Desktop\1.10AI学科体系\revelio_burst_test_cip6.csv')
df1 = df1.drop(df1.columns[[0, -1]], axis=1)
df1 = df1.rename(columns={'best_match': 'cip6_name'})
df1 = df1.groupby(['year', 'cip6_name']).size().reset_index(name='count')
df2=pd.read_csv(r'C:/Users/80577/Desktop/1.10AI学科体系/2.20/burst_test_cip6.csv')
df2 = df2.drop(df2.columns[[0]], axis=1)
df2 = df2.groupby(['year', 'cip6_name']).size().reset_index(name='count')
df2['cip6_name'] = df2['cip6_name'].apply(lambda x: x.lower().replace('/', ' '))

# Step 1: 使用 StandardScaler 对 revelio 和 lightcast 的 'count' 列进行标准化
scaler = StandardScaler()

# 对 df1 和 df2 的 'count' 列进行标准化
df1['count_scaled'] = scaler.fit_transform(df1[['count']])  # 对 revelio 数据进行拟合和转换
df2['count_scaled'] = scaler.transform(df2[['count']])    # 对 lightcast 数据进行转换

# Step 2: 合并 df1 和 df2 按 'year' 和 'cip6_name'
merged_df = pd.merge(df1, df2, on=['year', 'cip6_name'], suffixes=('_revelio', '_lightcast'))

# Step 3: 设置最大滞后阶数为 1
max_lag = 1

# Step 4: 创建字典来存储因果关系的结果
results = defaultdict(list)

# Step 5: 对于每个 'cip6_name' 进行格兰杰因果检验
cip6_names = merged_df['cip6_name'].unique()

for cip6_name in cip6_names:
    try:
        # 选取当前 'cip6_name' 的数据
        subset = merged_df[merged_df['cip6_name'] == cip6_name]
        
        # 确保数据列有足够的变化（去掉常数列）
        subset = subset[['count_scaled_revelio', 'count_scaled_lightcast']]  # 只选择需要的列
        
        # 再次检查常数列
        if subset.nunique().min() == 1:
            print(f"Skipping {cip6_name} because one of the columns is constant after filtering.")
            continue  # 跳过此学科
        
        # 格兰杰因果检验：revelio 的 count 是否能够预测 lightcast 的 count
        data = subset
        rs1 = grangercausalitytests(data, max_lag, verbose=False)

        # 格兰杰因果检验：lightcast 的 count 是否能够预测 revelio 的 count
        rs2 = grangercausalitytests(list(zip(data['count_scaled_lightcast'], data['count_scaled_revelio'])), max_lag, verbose=False)

        # 提取因果检验的统计量
        f_revelio2lightcast = rs1[1][0]['params_ftest'][0]  # F统计量（revelio -> lightcast）
        p_revelio2lightcast = rs1[1][0]['params_ftest'][1]  # p值（revelio -> lightcast）
        beta_revelio2lightcast = rs1[1][1][1].params[1]   # 贝塔系数（revelio -> lightcast）

        f_lightcast2revelio = rs2[1][0]['params_ftest'][0]  # F统计量（lightcast -> revelio）
        p_lightcast2revelio = rs2[1][0]['params_ftest'][1]  # p值（lightcast -> revelio）
        beta_lightcast2revelio = rs2[1][1][1].params[1]    # 贝塔系数（lightcast -> revelio）

        lrt_revelio2lightcast = rs1[1][0]['lrtest'][1]  # 似然比检验（revelio -> lightcast）
        lrt_lightcast2revelio = rs2[1][0]['lrtest'][1]  # 似然比检验（lightcast -> revelio）

        # 将统计量添加到结果字典中
        results[cip6_name] = [beta_revelio2lightcast, f_revelio2lightcast, p_revelio2lightcast, lrt_revelio2lightcast,
                              beta_lightcast2revelio, f_lightcast2revelio, p_lightcast2revelio, lrt_lightcast2revelio]

    except Exception as e:
        # 捕获异常并继续处理下一个 'cip6_name'
        print(f"Error processing {cip6_name}: {e}")
        continue  # 继续下一个学科

# Step 6: 将结果转化为 DataFrame
results_df = pd.DataFrame(results, index=['beta_revelio2lightcast', 'f_revelio2lightcast', 'p_revelio2lightcast', 'lrt_revelio2lightcast',
                                          'beta_lightcast2revelio', 'f_lightcast2revelio', 'p_lightcast2revelio', 'lrt_lightcast2revelio'])

# 显示结果
print(results_df)

rl = results_df.iloc[2]
a = rl[rl < 0.05]
lr = results_df.iloc[6]
b = lr[lr < 0.05]
c = results_df.iloc[1]
d = results_df.iloc[5]
a= a.reset_index().rename(columns={'index': 'cip6_name', 0: 'rl_p'})
b = b.reset_index().rename(columns={'index': 'cip6_name', 0: 'lr_p'})
c = c.reset_index().rename(columns={'index': 'cip6_name', 0: 'rl_f'})
d = d.reset_index().rename(columns={'index': 'cip6_name', 0: 'lr_f'})
a=pd.merge(a,c,how='left')
b=pd.merge(b,d,how='left')
granger=pd.merge(a,b,how='outer')
granger.to_csv(r'C:\Users\80577\Desktop\1.10AI学科体系\2.20\granger.csv')
