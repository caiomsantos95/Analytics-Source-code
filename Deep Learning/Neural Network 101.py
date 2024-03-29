# -*- coding: utf-8 -*-
"""HODL-Lecture 2-Heart-Disease-Prediction

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1f9jJDN0GH2pbGr_fo0WO3I_u4kRNOoGD

# Binary Classification on Tabular Data - Predicting Heart Disease

## Introduction

This colab shows to set up and train a Neural Network model for *binary classification*, when the dataset is *tabular* (rather than unstructured data like images or text) and has a mix of numeric and categorical features. Since tabular datasets are often made available in CSV files, the colab demonstrates the full CSV-to-trained-model workflow.




### The dataset

The dataset ([more background on the data](https://archive.ics.uci.edu/ml/datasets/heart+Disease)) has information on 303 patients, one in each row. Each column (i.e., feature) contains information on a particular attribute of the patient. The column named "Target" indicates if the patient has been diagnosed with heart disease or not and is the label (i.e., the dependent variable) that we want to predict using the other columns. 

Feature description (copied from [here](https://keras.io/examples/structured_data/structured_data_classification_from_scratch/)):

Column| Description| Feature Type
------------|--------------------|----------------------
Age | Age in years | Numerical
Sex | (1 = male; 0 = female) | Categorical
CP | Chest pain type (0, 1, 2, 3, 4) | Categorical
Trestbpd | Resting blood pressure (in mm Hg on admission) | Numerical
Chol | Serum cholesterol in mg/dl | Numerical
FBS | fasting blood sugar in 120 mg/dl (1 = true; 0 = false) | Categorical
RestECG | Resting electrocardiogram results (0, 1, 2) | Categorical
Thalach | Maximum heart rate achieved | Numerical
Exang | Exercise induced angina (1 = yes; 0 = no) | Categorical
Oldpeak | ST depression induced by exercise relative to rest | Numerical
Slope | Slope of the peak exercise ST segment | Numerical
CA | Number of major vessels (0-3) colored by fluoroscopy | Both numerical & categorical
Thal | 3 = normal; 6 = fixed defect; 7 = reversible defect | Categorical
Target | Diagnosis of heart disease (1 = true; 0 = false) | Target

## Technical preliminaries

Throughout the course, we will load the following packages as the first step and set the seed for different random number generators.
"""

import tensorflow as tf
from tensorflow import keras
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# initialize the seeds of different random number generators so that the 
# results will be the same every time the notebook is run
keras.utils.set_random_seed(42)

"""## Read in the data

Conveniently, the dataset in CSV form has been made available online (by [Francois Chollet](https://twitter.com/fchollet)) and we can load it into a Pandas dataframe with the very useful `pd.read_csv` command.
"""

df = pd.read_csv("http://storage.googleapis.com/download.tensorflow.org/data/heart.csv")

df.shape

"""The dataset has 303 rows and 14 columns (13 independent variables + 1 dependent variable):

Let's take a look at the first few rows:
"""

df.head()

"""## Preprocessing 

This dataset has both categorical variables and numeric variables.

We'd like to preprocess them as follows:
- *one-hot encode* the categorical variables
- *normalize* the numeric variables

It will be convenient (for later processing) to collect these groups of variables into two lists.
"""

categorical_variables = ['sex', 'cp', 'fbs', 'restecg','exang', 'ca', 'thal']
numerics = ['age', 'trestbps','chol', 'thalach', 'oldpeak', 'slope']

"""
With the pandas `get_dummies` function, you can one-hot-encode in one line.
"""

df = pd.get_dummies(df, columns = categorical_variables)

df.head()

"""

> Indented block


Before we normalize the numerics, let's split the data into an 80% training set and 20% test set (*why should we split **before** normalization?*)."""

test_df = df.sample(frac=0.2, random_state=42)
train_df = df.drop(test_df.index)

train_df.shape

test_df.shape

"""OK, let's calculate the mean and standard deviation of every numeric variable in the training set."""

means = train_df[numerics].mean()
sd = train_df[numerics].std()

means

"""Let's normalize the train and test dataframes with these means and standard deviations."""

train_df[numerics]= (train_df[numerics] - means)/sd

test_df[numerics]= (test_df[numerics] - means)/sd

train_df.head()

"""The easiest way to feed data to Keras/Tensorflow is as Numpy arrays so we convert our two dataframes."""

train = train_df.to_numpy()
test = test_df.to_numpy()

"""At this point, our features $X$ and dependent variable $y$ are both inside the arrays so let's separate them out."""

train_X = train[:,: -1]
train_y = train[:, -1]

test_X = test[:,: -1]
test_y = test[:, -1]

train_X.shape, train_y.shape

test_X.shape, test_y.shape

"""## Build a model

### Define model in Keras

Creating an NN  is usually just a few lines of Keras code. 

* We will start with a single hidden layer. 
* Since this is a *binary classification problem*, we will use a sigmoid activation in the output layer.
"""

num_columns = train_X.shape[1]

# define the input layer
input = keras.Input(shape=num_columns)

# feed the input vector to the hidden layer
h = keras.layers.Dense(16, activation="relu", name="Hidden")(input)

# feed the output of the hidden layer to the output layer
output = keras.layers.Dense(1, activation="sigmoid", name="Output")(h)

# tell Keras that this (input,output) pair is your model
model = keras.Model(input, output)

"""The `model.summary()` command is a good way to get a quick overview of what you have defined."""

model.summary()

"""We can "visualize" the network graphically as well using Keras' `plot_model` function."""

keras.utils.plot_model(model, show_shapes=True)

"""Let's hand-calculate the number of parameters to verify."""

(29 * 16 + 16) + (16 * 1 + 1)

"""### Set optimization parameters

Now that the model is defined, we need to tell Keras three things:

*   What **loss function** to use - Since our output variable is binary, we will select the `binary_crossentropy` loss function. 
*   Which **optimizer** to use - we will use a sibling of SGD called **Adam** which is an excellent default choice 
*   What **metrics** you want Keras to report out - in classification problems like this one, Accuracy is usually the metric you want to see.
"""

model.compile(optimizer="adam",
              loss="binary_crossentropy",
              metrics=["accuracy"])

"""## Train the model

To kickoff training, we have to decide on three things:
* The *batch size* - 32 is a good default
* The number of *epochs* i.e., how many passes through the training data. Usually 20-30 epochs is a good starting point but since this dataset is very small, each epoch will be very quick so let's run it for 300 epochs. It will allow us to see if any overfitting happens
* Whether we want to use a validation set. This will be useful for overfitting detection and regularization via early stopping so we will ask Keras to automatically use 20% of the data points as a validation set

OK, let's train the model using the `model.fit` function!
"""

history = model.fit(train_X,
                    train_y,
                    epochs=300,
                    batch_size=32,
                    validation_split=0.2)

"""We get to over 95% validation accuracy!

Let's plot the training and loss curves to see if ***overfitting*** is going on and if **early stopping** may be needed.
"""

history_dict = history.history
history_dict.keys()

loss_values = history_dict["loss"]
val_loss_values = history_dict["val_loss"]
epochs = range(1, len(loss_values) + 1)
plt.plot(epochs, loss_values, "bo", label="Training loss")
plt.plot(epochs, val_loss_values, "b", label="Validation loss")
plt.title("Training and validation loss")
plt.xlabel("Epochs")
plt.ylabel("Loss")
plt.legend()
plt.show()

"""Maybe a little overfitting starts between 150-200 epochs. 

We could go back, re-initialize the model and just run it for 150 epochs - that would an example of **early stopping**.

Let's look at the accuracy curves as well.

"""

plt.clf()
acc = history_dict["accuracy"]
val_acc = history_dict["val_accuracy"]
plt.plot(epochs, acc, "bo", label="Training acc")
plt.plot(epochs, val_acc, "b", label="Validation acc")
plt.title("Training and validation accuracy")
plt.xlabel("Epochs")
plt.ylabel("Accuracy")
plt.legend()
plt.show()

"""The overfitting we saw on the loss curves isn't visible here so we will just move on to evaluating the model on the test set.

Feel free to re-initialize the model and just run it for 150 epochs and see what happens.

## Evaluate the model

Let's see **how well the model does on the test set**. 

`model.evaluate` is a very handy function to calculate the performance of your model on any dataset.
"""

model.evaluate(test_X, test_y)

"""Nice!! But don't be impressed - this is a tiny dataset (61 rows!) and it is not hard to do well."""

test_X.shape

"""## Predicting new data (i.e., *inference*) with the model

We will see in later colabs how to save a Keras model and use it for prediction later.  But we want to point out that the simple approach we followed in this colab has a key shortcoming.

We did the pre-processing - the one-hot encoding and normalization - *outside* the model. This means that we have to remember what pre-processing we did and carry that information (e.g., the mean and variance of each variable) along with the model to correctly use the model in the future.

A very elegant way to avoid this issue is to use [Keras preprocessing layers](https://keras.io/guides/preprocessing_layers/). In the interest of time, we aren't covering it in this colab but we encourage you to check out this [colab](https://colab.research.google.com/github/keras-team/keras-io/blob/master/examples/structured_data/ipynb/structured_data_classification_from_scratch.ipynb) which shows how to solve our exact heart-disease prediction problem above using preprocessing layers.

"""
