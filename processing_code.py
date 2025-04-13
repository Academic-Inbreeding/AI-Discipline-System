import pymysql
import pandas as pd
from tqdm import tqdm
import os


db_config = {
    'user': 'root',
    'password': 'V7MyaKAm31w2dEamFMkX',
    'host': 'localhost',
    'port': 3306,
    'database': 'import1',
    'charset': 'utf8mb4'
}


COLUMNS = [
    'id', 'city_name', 'company', 'education_levels', 'min_edulevels', 'max_edulevels',
    'employment_type', 'min_years_experience', 'max_years_experience', 'county', 'msa',
    'state', 'naics6', 'title_clean', 'salary', 'skills', 'specialized_skills',
    'certifications_name', 'common_skills', 'onet', 'salary_to', 'salary_from',
    'cip6', 'cip6_name', 'cip4', 'cip4_name', 'cip2', 'cip2_name','is_ai_broad', 'year', '`is.gpu`',
    '`is.dl`', '`is.ai`', '`is.ai.narrow`', '`is.ml`', '`is.cloud.narrow`',
    '`is.cloud.broad`', '`is.big.data`',  '`is.robotics`']

connection = pymysql.connect(**db_config)

try:
    with connection.cursor() as cursor:
        cursor.execute("SHOW TABLES")
        tables = [table[0] for table in cursor.fetchall() if table[0].startswith('all_for')]

    os.makedirs('E:/title', exist_ok=True)


    batch_size = 12
    batch_dfs = []
    file_index = 1

    for i in tqdm(range(0, len(tables), batch_size), desc='Processing Batches'):
        batch_tables = tables[i:i + batch_size]
        batch_data = []

        for table in tqdm(batch_tables, desc=f'Processing {file_index} Batch', leave=False):
            try:
                query = f"SELECT {', '.join(COLUMNS)} FROM `{table}`"
                df = pd.read_sql(query, connection)
                batch_data.append(df)
            except Exception as e:
                print(f"Error processing table {table}: {e}")

        if batch_data:
            combined_df = pd.concat(batch_data, ignore_index=True)
            combined_df.to_parquet(f'E:/title/batch_{file_index}.parquet')
            file_index += 1

finally:
    connection.close()

##################################
import pymysql
import pandas as pd
from tqdm import tqdm
import os

db_config = {
    'user': 'root',
    'password': 'V7MyaKAm31w2dEamFMkX',
    'host': 'localhost',
    'port': 3306,
    'database': 'import1',
    'charset': 'utf8mb4'
}

# 连接数据库
connection = pymysql.connect(**db_config)

try:
    with connection.cursor() as cursor:
        # 查询所有以'all_for'开头的表名
        cursor.execute("SHOW TABLES")
        tables = [table[0] for table in cursor.fetchall() if table[0].startswith('all_for_2010')]

    os.makedirs('E:/title', exist_ok=True)

    for table in tqdm(tables, desc='Processing Tables'):
        try:
            # 查询skills列数据
            query = f"SELECT `skills` FROM `{table}`"
            with connection.cursor() as cursor:
                cursor.execute(query)
                result = cursor.fetchall()
                skills_list = [row[0] for row in result if row[0]]  # 排除空值

            # 统计skills频次
            skill_counts = {}
            for skills in skills_list:
                for skill in skills.split('|'):
                    skill = skill.strip()
                    if skill:
                        skill_counts[skill] = skill_counts.get(skill, 0) + 1

            # 转换为DataFrame并保存
            skills_df = pd.DataFrame(list(skill_counts.items()), columns=['skill', 'count'])
            skills_df.to_parquet(f'E:/title/skills_{table}.parquet')

        except Exception as e:
            print(f"Error processing table {table}: {e}")

finally:
    connection.close()

import pandas as pd
import glob
import os
from tqdm import tqdm

# 定义文件路径
folder_path = r'E:\title'
parquet_files = glob.glob(os.path.join(folder_path, '*.parquet'))

# 假设 df_merged 已经存在，包含 'skill' 和 'ai_score' 列
# 将 skill 和 ai_score 转换为字典以便快速查找
ai_score_dict = df_merged.set_index('skill')['ai_score'].to_dict()

# 处理每个 parquet 文件，使用 tqdm 展示进度条
for file in tqdm(parquet_files, desc="Processing Parquet Files", unit="file"):
    # 读取 parquet 文件
    df = pd.read_parquet(file)
    
    # 计算 mean_ai_score
    def calculate_mean_ai_score(skills):
        # 分割 skills 列（以 '|' 分割）
        skill_list = str(skills).split('|')
        
        # 提取对应的 ai_score，忽略找不到的 skill
        scores = [ai_score_dict.get(skill, None) for skill in skill_list if ai_score_dict.get(skill, None) is not None]
        
        # 计算均值
        return sum(scores) / len(scores) if scores else 0

    # 应用计算函数（使用 tqdm 监控每个文件内部的进度）
    df['mean_ai_score'] = [calculate_mean_ai_score(skills) for skills in tqdm(df['skills'], desc=f"Calculating in {os.path.basename(file)}", leave=False)]

    # 保存回原路径
    df.to_parquet(file, index=False)


#################################################################################计算相对重要性得分


# 创建一个字典，用于记录每个 cip6_name 类别的总分数和出现次数
category_scores = {}

# 使用 tqdm 包装循环，显示进度条
for row in tqdm(df['cip6_name'], desc="Processing rows"):
    # 分割文本，得到每个类别的列表
    categories = row.split('|')
    category_count = len(categories)  # 当前行中类别的数量

    # 为每个类别分配相应的分数（总分，而不是均分）
    for category in categories:
        if category not in category_scores:
            category_scores[category] = 0
        
        category_scores[category] += 1 / category_count
cip6_score = pd.DataFrame(list(category_scores.items()), columns=['cip6_name', 'total_score'])       
df_ai = df[df['mean_ai_score'] >= 0.1]
category_scores = {}

# 使用 tqdm 包装循环，显示进度条
for row in tqdm(df_ai['cip6_name'], desc="Processing rows"):
    # 分割文本，得到每个类别的列表
    categories = row.split('|')
    category_count = len(categories)  # 当前行中类别的数量

    # 为每个类别分配相应的分数（总分，而不是均分）
    for category in categories:
        if category not in category_scores:
            category_scores[category] = 0
        
        category_scores[category] += 1 / category_count
cip6_score_ai = pd.DataFrame(list(category_scores.items()), columns=['cip6_name', 'total_score'])       

# 创建一个字典，用于记录每个 cip6_name 类别的总分数和出现次数
category_scores = {}

# 使用 tqdm 包装循环，显示进度条
for row in tqdm(df['cip2_name'], desc="Processing rows"):
    # 分割文本，得到每个类别的列表
    categories = row.split('|')
    category_count = len(categories)  # 当前行中类别的数量

    # 为每个类别分配相应的分数（总分，而不是均分）
    for category in categories:
        if category not in category_scores:
            category_scores[category] = 0
        
        category_scores[category] += 1 / category_count
cip2_score = pd.DataFrame(list(category_scores.items()), columns=['cip2_name', 'total_score'])       
df_ai = df[df['mean_ai_score'] >= 0.1]
category_scores = {}

# 使用 tqdm 包装循环，显示进度条
for row in tqdm(df_ai['cip2_name'], desc="Processing rows"):
    # 分割文本，得到每个类别的列表
    categories = row.split('|')
    category_count = len(categories)  # 当前行中类别的数量

    # 为每个类别分配相应的分数（总分，而不是均分）
    for category in categories:
        if category not in category_scores:
            category_scores[category] = 0
        
        category_scores[category] += 1 / category_count
cip2_score_ai = pd.DataFrame(list(category_scores.items()), columns=['cip2_name', 'total_score'])    


###########################################################################################计算学科互补
import pandas as pd
from itertools import combinations
from collections import defaultdict


all_cip6_names = df_ai['cip6_name'].str.split('|').explode()
cip6_counts = all_cip6_names.value_counts().reset_index()
cip6_counts.columns = ['cip6_name', 'count']


# 遍历每一行
pair_counts = defaultdict(int)
for row in df_ai['cip6_name']:
    elements = row.split('|')
    if len(elements) > 1:
        for pair in combinations(elements, 2):
            sorted_pair = tuple(sorted(pair))
            pair_counts[sorted_pair] += 1

# 将 pair 拆分为两列 x 和 y
b = pd.DataFrame(list(pair_counts.items()), columns=['pair', 'b_count'])
b[['x', 'y']] = pd.DataFrame(b['pair'].tolist(), index=b.index)
b = b.drop(columns=['pair'])  # 删除原始的 pair 列

a_dict = cip6_counts.set_index('cip6_name')['count'].to_dict()

# 定义一个函数来计算新变量
def calculate_new_variable(row):
    x_count = a_dict.get(row['x'], 0)  # 获取 x 的出现次数
    y_count = a_dict.get(row['y'], 0)  # 获取 y 的出现次数
    return x_count + y_count - row['b_count']

# 应用函数计算新变量
b['all_count'] = b.apply(calculate_new_variable, axis=1)
b['common']=b['b_count']/b['all_count']
b.to_csv(r'E:/title\cip6_common.csv')
a.to_csv(r'E:/title\cip6_one.csv')
######################################
df['naics2'] = df['naics6'].astype(str).str[:2]
df['onet2'] = df['onet'].astype(str).str[:2]
from tqdm import tqdm
import pandas as pd

# 初始化函数：计算 category_scores 并新增两列
def calculate_category_scores(df, group_column, score_column='cip2_name'):
    grouped_category_scores = {}
    group_row_counts = {}  # 用于存储每个分组的行数
    
    # 按照指定列分组
    grouped = df.groupby(group_column)
    
    # 使用 tqdm 包装循环，显示进度条
    for group_value, group in tqdm(grouped, desc=f"Processing {group_column} groups"):
        category_scores = {}  # 初始化当前分组的 category_scores
        group_row_counts[group_value] = len(group)  # 记录当前分组的行数
        
        # 遍历当前分组中的每一行
        for row in group[score_column]:
            # 分割文本，得到每个类别的列表
            categories = row.split('|')
            category_count = len(categories)  # 当前行中类别的数量
            
            # 为每个类别分配相应的分数（总分，而不是均分）
            for category in categories:
                if category not in category_scores:
                    category_scores[category] = 0
                
                category_scores[category] += 1 / category_count
        
        # 将当前分组的 category_scores 存储到 grouped_category_scores 中
        grouped_category_scores[group_value] = {
            'scores': category_scores,
            'row_count': group_row_counts[group_value]  # 存储当前分组的行数
        }
    
    # 将 grouped_category_scores 转换为 DataFrame
    results = []
    for group_value, data in grouped_category_scores.items():
        row_count = data['row_count']  # 获取当前分组的行数
        scores = data['scores']
        for category, score in scores.items():
            normalized_score = score / row_count  # 计算标准化分数
            results.append([group_value, category, score, row_count, normalized_score])
    
    # 创建最终的 DataFrame
    result_df = pd.DataFrame(results, columns=[group_column, 'cip2_name', 'total_score', 'row_count', 'normalized_score'])
    return result_df

# 按照 onet2 分组计算两个结果数据集
cip2_score_onet2 = calculate_category_scores(df, group_column='onet2')
cip2_score_ai_onet2 = calculate_category_scores(df[df['mean_ai_score'] >= 0.1], group_column='onet2')

# 按照 naics2 分组计算两个结果数据集
cip2_score_naics2 = calculate_category_scores(df, group_column='naics2')
cip2_score_ai_naics2 = calculate_category_scores(df[df['mean_ai_score'] >= 0.1], group_column='naics2')

cip2_score_year = calculate_category_scores(df, group_column='year')
cip2_score_ai_year = calculate_category_scores(df[df['mean_ai_score'] >= 0.1], group_column='year')

cip2_score_state = calculate_category_scores(df, group_column='state')
cip2_score_ai_state = calculate_category_scores(df[df['mean_ai_score'] >= 0.1], group_column='state')

from collections import defaultdict
from itertools import combinations
import pandas as pd

# 初始化函数：按照 onet2 分组计算类别和类别对的统计信息
def calculate_grouped_stats(df, group_column='onet2', score_column='cip6_name'):
    # 存储每个分组的最终结果
    grouped_results = []
    
    # 按照指定列分组
    grouped = df.groupby(group_column)
    
    # 遍历每个分组
    for group_value, group in grouped:
        # 统计每个 cip6_name 的出现次数
        all_cip6_names = group[score_column].str.split('|').explode()
        cip6_counts = all_cip6_names.value_counts().reset_index()
        cip6_counts.columns = ['cip6_name', 'count']
        
        # 统计类别对的共现次数
        pair_counts = defaultdict(int)
        for row in group[score_column]:
            elements = row.split('|')
            if len(elements) > 1:
                for pair in combinations(elements, 2):
                    sorted_pair = tuple(sorted(pair))
                    pair_counts[sorted_pair] += 1
        
        # 将类别对转换为 DataFrame
        b = pd.DataFrame(list(pair_counts.items()), columns=['pair', 'b_count'])
        b[['x', 'y']] = pd.DataFrame(b['pair'].tolist(), index=b.index)
        b = b.drop(columns=['pair'])  # 删除原始的 pair 列
        
        # 将 cip6_counts 转换为字典，方便快速查找
        a_dict = cip6_counts.set_index('cip6_name')['count'].to_dict()
        
        # 计算新变量 all_count 和 common
        def calculate_new_variable(row):
            x_count = a_dict.get(row['x'], 0)  # 获取 x 的出现次数
            y_count = a_dict.get(row['y'], 0)  # 获取 y 的出现次数
            return x_count + y_count - row['b_count']
        
        b['all_count'] = b.apply(calculate_new_variable, axis=1)
        b['common'] = b['b_count'] / b['all_count']
        
        # 添加分组列
        b[group_column] = group_value
        
        # 将当前分组的结果添加到 grouped_results 中
        grouped_results.append(b)
    
    # 合并所有分组的结果
    final_result = pd.concat(grouped_results, ignore_index=True)
    return final_result

# 按照 onet2 分组计算统计信息
grouped_stats1 = calculate_grouped_stats(df_ai, group_column='onet2')
grouped_stats2 = calculate_grouped_stats(df_ai, group_column='naics2')
grouped_stats3 = calculate_grouped_stats(df_ai, group_column='year')
grouped_stats4 = calculate_grouped_stats(df_ai, group_column='state')

df['startdate'] = pd.to_datetime(df['startdate'])
df['startyear'] = df['startdate'].dt.year
df_us=df
df = df[['startyear', 'endyear', 'best_match','is_ai_broad']]
result_df = pd.DataFrame(columns=['year', 'best_match', 'match_score', 'is_ai_broad'])
df0=df.iloc[:1000]
result_df = pd.DataFrame(columns=['year', 'best_match', 'match_score', 'is_ai_broad'])
df['startyear'] = df['startyear'].astype(int)
df['endyear'] = df['endyear'].astype(int)
df['startyear'] = df['startyear'].apply(lambda x: 2010 if x < 2010 else x)
# 将 endyear 大于等于 2023 的值修改为 2023
df['endyear'] = df['endyear'].apply(lambda x: 2023 if x >= 2023 else x)
# 使用 tqdm 包装循环以显示进度条
result_df = pd.DataFrame(columns=['year', 'best_match', 'is_ai_broad'])
data_list = []

# 生成所有年份的列表，并将其与其他列的数据结合
for _, row in tqdm(df.iterrows(), total=len(df), desc="Processing rows"):
    start_year = row['startyear']
    end_year = row['endyear']
    
    # 生成从 start_year 到 end_year 的年份列表
    years = range(start_year, end_year + 1)
    
    # 将这些年份以及相关列的数据添加到列表中
    for year in years:
        data_list.append({
            'year': year,
            'best_match': row['best_match'],
            'is_ai_broad': row['is_ai_broad']
        })
result_df = pd.DataFrame(data_list)

# 重置索引（可选）
result_df.reset_index(drop=True, inplace=True)
category_count = result_df['best_match'].value_counts()

# 统计is_ai_broad=1的样本中，best_match各类别的数量
filtered_df = result_df[result_df['is_ai_broad'] == 1]
category_count_filtered = filtered_df['best_match'].value_counts()

# 合并这两个结果
ai_score = pd.DataFrame({
    'count_all': category_count,
    'count_ai_broad': category_count_filtered
}).fillna(0) 
ai_score.to_csv(r'revelio_ai_score.csv') 
filtered_df.to_csv(r'revelio_burst_test_cip6.csv')
df.to_parquet(r'E:\title\df_us.parquet')