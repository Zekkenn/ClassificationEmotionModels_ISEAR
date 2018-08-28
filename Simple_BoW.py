#Simple Softmax classificator - extracted and adapted from https://github.com/tensorflow/tensorflow/blob/master/tensorflow/examples/learn/text_classification.py
from py_isear_dataset.py_isear.isear_loader import IsearLoader

import tensorflow as tf
import tensorflow.contrib.learn as learn
import itertools
import numpy as np
import sys 
from sklearn import metrics

n_words = 0
MAX_LABEL = 8
MAX_DOCUMENT_LENGTH = 50
WORDS_FEATURE = 'words'
EMBEDDING_SIZE = 50


def estimator_spec_for_softmax_classification(logits, labels, mode):
  """Returns EstimatorSpec instance for softmax classification."""
  predicted_classes = tf.argmax(logits, 1)
  if mode == tf.estimator.ModeKeys.PREDICT:
    return tf.estimator.EstimatorSpec(
        mode=mode,
        predictions={
            'class': predicted_classes,
            'prob': tf.nn.softmax(logits)
        })

  loss = tf.losses.sparse_softmax_cross_entropy(labels=labels, logits=logits)
  if mode == tf.estimator.ModeKeys.TRAIN:
    optimizer = tf.train.AdamOptimizer(learning_rate=0.01)
    train_op = optimizer.minimize(loss, global_step=tf.train.get_global_step())
    return tf.estimator.EstimatorSpec(mode, loss=loss, train_op=train_op)

  eval_metric_ops = {
      'accuracy':
          tf.metrics.accuracy(labels=labels, predictions=predicted_classes)
  }
  return tf.estimator.EstimatorSpec(
      mode=mode, loss=loss, eval_metric_ops=eval_metric_ops)


def bag_of_words_model(features, labels, mode):
  """A bag-of-words model. Note it disregards the word order in the text."""
  bow_column = tf.feature_column.categorical_column_with_identity(
      WORDS_FEATURE, num_buckets=n_words)
  bow_embedding_column = tf.feature_column.embedding_column(
      bow_column, dimension=EMBEDDING_SIZE)
  bow = tf.feature_column.input_layer(
      features, feature_columns=[bow_embedding_column])
  logits = tf.layers.dense(bow, MAX_LABEL, activation=None)
  return estimator_spec_for_softmax_classification(
      logits=logits, labels=labels, mode=mode)

def main():
    global n_words, MAX_DOCUMENT_LENGTH
    tf.logging.set_verbosity(tf.logging.INFO)

    #ISEAR LOAD
    data = ['TEMPER', 'TROPHO']
    target = ['EMOT']
    loader = IsearLoader(data,target)
    dataset = loader.load_isear('py_isear_dataset/isear.csv')

    text_data_set = dataset.get_freetext_content()
    target_set = dataset.get_target()
    target_chain = itertools.chain(*target_set)
    target_data = list(target_chain)

    #Prepare training and testing data
    x_train = text_data_set[:round(len(text_data_set)*0.7)]
    y_train = target_data[:round(len(text_data_set)*0.7)]
    x_test = text_data_set[round(len(text_data_set)*0.7):]
    y_test = target_data[round(len(text_data_set)*0.7):]

    print("Initial Data Set:")
    print(len(text_data_set))
    print(len(x_train))
    print(len(x_test))

    # Process vocabulary
    vocab_processor = learn.preprocessing.VocabularyProcessor(
          MAX_DOCUMENT_LENGTH)

    x_transform_train = vocab_processor.fit_transform(x_train)
    x_transform_test = vocab_processor.transform(x_test)

    x_train = np.array(list(x_transform_train))
    x_test = np.array(list(x_transform_test))
    y_train = np.array(y_train)
    y_test = np.array(y_test)


    n_words = len(vocab_processor.vocabulary_)
    print('Total words: %d' % n_words)

    x_train -=1
    x_test -=1
    classifier = tf.estimator.Estimator(model_fn=bag_of_words_model)

    # Train.
    train_input_fn = tf.estimator.inputs.numpy_input_fn(
      x={WORDS_FEATURE: x_train},
      y=y_train,
      batch_size=len(x_train),
      num_epochs=None,
      shuffle=True)
    classifier.train(input_fn=train_input_fn, steps=100)


    # Predict.
    test_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={WORDS_FEATURE: x_test}, y=y_test, num_epochs=1, shuffle=False)
    predictions = classifier.predict(input_fn=test_input_fn)
    y_predicted = np.array(list(p['class'] for p in predictions))
    y_predicted = y_predicted.reshape(np.array(y_test).shape)

    # Score with sklearn.
    score = metrics.accuracy_score(y_test, y_predicted)
    print('Accuracy (sklearn): {0:f}'.format(score))

    # Score with tensorflow.
    scores = classifier.evaluate(input_fn=test_input_fn)
    print('Accuracy (tensorflow): {0:f}'.format(scores['accuracy']))



main()
