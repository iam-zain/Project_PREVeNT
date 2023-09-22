import os
import sys
import joblib
import warnings
import numpy as np
import pandas as pd
import tensorflow as tf
from scipy.stats import pearsonr
from keras.layers import Input, Dense
from keras.models import load_model, Model
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import pairwise_distances
from tensorflow.keras.models import Sequential
from sklearn.model_selection import train_test_split
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics import confusion_matrix, accuracy_score
warnings.filterwarnings("ignore")
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

os.chdir ('Z:\PPMI_Data\Excels\CollaborativeFiltering')
df = pd.read_csv('Feats45_unCategAge_APPRDX.csv')
df1 = df.drop(['PATNO','APPRDX'], axis = 1)
df2 = pd.read_csv('NonMotor_Empty.csv')
df3 = df2.drop('Patient_ID', axis = 1)
new_age = int(sys.argv[1])
df3.at[0, 'Age'] = new_age
col_age = df3.pop('Age')
df3.insert(0, "Age", col_age)
dframe1 = pd.read_csv('Feats45_unCategSparse_APPRDX.csv')
dframe = dframe1.drop(['PATNO','APPRDX','Age'], axis = 1)
dframe = dframe.add(1)
tests_scores = []
num_columns = int(sys.argv[2])
def update_values(df3, dframe):
    # Input from the user for number of columns
    num_columns = int(sys.argv[2])
    if num_columns < 3 or num_columns > 45:
        print("Invalid input, please enter a number from 3 to 45")
        sys.exit()

    columns = []
    values = []

    for i in range(num_columns):
        column = sys.argv[i*2 + 3]
        value = int(sys.argv[i*2 + 4])
        columns.append(column)
        values.append(value)
        tests_scores.append([column, value])
    for i in range(num_columns):
        df3.loc[df3[columns[i]] != values[i], columns[i]] = values[i] 
    for col in dframe.columns:
        if col not in columns and col != 'Age': 
            dframe = dframe.drop(col, axis=1)
    
    return df3, dframe

df3, dframe = update_values(df3, dframe)
col_age = df3.pop('Age')
df3.insert(0, "Age", col_age)
df3 = df3.dropna(axis = 1)
dframe = dframe.dropna()
col_age = dframe1.pop('Age')
dframe.insert(0, "Age", col_age)
col_Apprdx = dframe1.pop('APPRDX')
dframe.insert(0, 'APPRDX', col_Apprdx)
dframes = dframe.drop(['APPRDX'], axis = 1)
dframe.loc[:, "APPRDX"] = dframe["APPRDX"].apply(lambda x: x - 1)
X = dframe.iloc[:, 1:].values
y = dframe.iloc[:, 0].values
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 1)

model_ann = load_model('model_ann.h5')
new_input_layer = Input(shape=dframes.shape[1:])
new_hidden_layer1 = Dense(units=12, activation='relu',kernel_regularizer=tf.keras.regularizers.l1(0.01))(new_input_layer)
pretrained_hidden_layer2_weights = model_ann.layers[1].get_weights()
pretrained_output_layer_weights = model_ann.layers[2].get_weights()
new_hidden_layer2 = Dense(units=pretrained_hidden_layer2_weights[0].shape[1], activation='relu', weights=pretrained_hidden_layer2_weights)(new_hidden_layer1)
new_output_layer = Dense(units=pretrained_output_layer_weights[0].shape[1], activation='sigmoid', weights=pretrained_output_layer_weights)(new_hidden_layer2)
new_model = Model(inputs=new_input_layer, outputs=new_output_layer)
new_model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

for layer in new_model.layers[2:]:
    layer.trainable = False
    
new_model.fit(X_train, y_train, batch_size = 32, epochs = 20, verbose = 0)
prediction = new_model.predict(df3, verbose = 0)
if prediction[0][0] > 0.5:
    predicted_class = "Healthy"
else:
    predicted_class = "Patient"
#print("The user might fall under category of", predicted_class)

#y_pred = new_model.predict(X_test)
#y_pred = (y_pred > 0.5)
#cm = confusion_matrix(y_test, y_pred)
#print(cm)
#accuracy_score(y_test, y_pred)

df4 = df3
df4.insert (0, 'Patient_ID', df2['Patient_ID'])
merged_df = pd.concat ([df1, df4], axis = 0)
merged_df.reset_index(inplace = True)
merged_df1 = merged_df.drop(['index','Patient_ID'], axis = 1)
scaler = MinMaxScaler (feature_range = (1,5))
df_scaled = scaler.fit_transform(merged_df1)
df_scaled = pd.DataFrame (df_scaled, columns = merged_df1.columns)
df_scaled.insert (0, 'Patient_ID', merged_df['Patient_ID'], True)
df_melted = df_scaled.melt(id_vars='Patient_ID', var_name='Feature', value_name='Value')
feat_rating_count = df_melted.groupby('Patient_ID')['Value'].count()
feat_rating_count = pd.DataFrame(feat_rating_count)
feat_rating_count.columns = ['Value_count']
feat_rating_count['Patient_ID'] = feat_rating_count.index
feat_rating_count = feat_rating_count.reset_index(drop=True)
dframe = df_melted.merge(feat_rating_count, on = 'Patient_ID', how = 'inner')
RatingMat = dframe.pivot_table(index=['Patient_ID'],columns=['Feature'],values=['Value'],fill_value=0)
Original_RatingMat = RatingMat.copy()
RatingMat.columns = RatingMat.columns.droplevel()
RatingMat_centered = RatingMat.sub(RatingMat.mean(axis=1), axis=0)
user_similarity = cosine_similarity(RatingMat_centered)
user_sim_df = pd.DataFrame(user_similarity, index=RatingMat.index, columns=RatingMat.index)
user_Eucsimilarity = 1 - pairwise_distances(RatingMat, metric="euclidean")
user_Eucsim_df = pd.DataFrame(user_Eucsimilarity, index=RatingMat.index, columns=RatingMat.index)
current_user_rating = dframe[(dframe.Patient_ID == 999) & (dframe.Value != 0)]['Feature']
current_user_rating = pd.DataFrame(current_user_rating, columns=['Feature'])
curr_user_similarity = pd.DataFrame(user_sim_df.loc[999])
curr_user_similarity.rename(columns={999:'Similarity_Score'},inplace=True)
curr_user_similarity.sort_values(by='Similarity_Score', ascending=False, inplace=True)
curr_user_similarity1 = curr_user_similarity.iloc[1:, :]
curr_user_similarity1.reset_index(inplace=True)
curr_user_similarity1.rename(columns={'index': 'Index_Column'}, inplace=True)

def categorize(m):
    if m < 165:
        return 'Patient'
    else:
        return 'Healthy'

curr_user_similarity1['Patient_Type'] = curr_user_similarity1['Patient_ID'].apply(categorize)
similar_user = curr_user_similarity1.iloc[:5, :]
sim_counts = similar_user['Patient_Type'].value_counts()
most_occur_value = sim_counts.index[0]
#print("The user might fall under category of", most_occur_value)

predictions = []
for i in range(num_columns):
    column = tests_scores[i][0]
    user_data = tests_scores[i][1]
    user_data = np.array(user_data).reshape(-1, 1)
    #adaBoost_model = joblib.load(column + '_AdaBoost_model.joblib')
    #adaBoost_prediction = adaBoost_model.predict(user_data)
    #print(f"For the test {column}, user may belong to: {adaBoost_prediction[0]} using AdaBoost model")
    LASSO_model = joblib.load(column + '_Lasso_model.joblib')
    Lasso_prediction = LASSO_model.predict(user_data)
    #print(f"For the test {column}, user may belong to: {Lasso_prediction[0]} using Lasso model")
    svmL_model = joblib.load(column + '_svmL_model.joblib')
    SVM_prediction = svmL_model.predict(user_data)
    #print(f"For the test {column}, user may belong to: {SVM_prediction[0]} using SVM model")
    enet_model = joblib.load(column + '_enet_model.joblib')
    enet_prediction = enet_model.predict(user_data)
    #print(f"For the test {column}, user may belong to: {enet_prediction[0]} using Elastic Net model")
    #Bag_model = joblib.load(column + '_bag_model.joblib')
    #Bag_prediction = Bag_model.predict(user_data)
    #print(f"For the test {column}, user may belong to: {Bag_prediction[0]} using Bagging model")
    predictions.append([Lasso_prediction[0], SVM_prediction[0],enet_prediction[0]])

feat_pred_df = pd.DataFrame(predictions, columns=["Lasso", "SVM Linear", "Elastic Net"])
counts = feat_pred_df.apply(pd.Series.value_counts, axis=1).fillna(0).astype(int)
result = counts.idxmax(axis=1)
max_inAll = pd.DataFrame()
max_inAll["Maximum_Occurrence"] = pd.DataFrame(result)
max_inAll = max_inAll.iloc[:, ]
counts = max_inAll['Maximum_Occurrence'].value_counts()
most_occurring_value = counts.index[0]
#print("The user might fall under category of", most_occurring_value)

df_res = pd.DataFrame({'most_occurring_value': [most_occurring_value],
                       'most_occur_value': [most_occur_value],
                       'predicted_class': [predicted_class]})

value_counts = df_res.apply(pd.value_counts).fillna(0)
highest_occurring_value = value_counts.idxmax().values[0]
print(f"The user might fall under category of {highest_occurring_value}")
