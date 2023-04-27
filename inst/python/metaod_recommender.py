# %%
import numpy as np

from sklearn.metrics import dcg_score, ndcg_score
from sklearn.utils import check_array
from sklearn.metrics import mean_squared_error

from sklearn.multioutput import MultiOutputRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.decomposition import PCA
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from copy import deepcopy


def get_mse(pred, actual):
    # Ignore nonzero terms.
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)


def sigmoid(x, a=1):
    return 1 / (1 + np.exp(-1 * a * x))


def sigmoid_derivate(x, a=1):
    return sigmoid(x, a) * (1 - sigmoid(x, a))


class MetaODClass(object):
    def __init__(self,
                 train_performance,
                 n_factors=40,
                 learning='sgd',
                 verbose=False):
        """
        Train a matrix factorization model to predict empty 
        entries in a matrix. The terminology assumes a 
        train_performance matrix which is ~ user x item
        
        Params
        ======
        train_performance : (ndarray)
            User x Item matrix with corresponding train_performance
        
        n_factors : (int)
            Number of latent factors to use in matrix 
            factorization model
        learning : (str)
            Method of optimization. Options include 
            'sgd' or 'als'.
        
        item_fact_reg : (float)
            Regularization term for item latent factors
        
        user_fact_reg : (float)
            Regularization term for user latent factors
            
        item_bias_reg : (float)
            Regularization term for item biases
        
        user_bias_reg : (float)
            Regularization term for user biases
        
        verbose : (bool)
            Whether or not to printout training progress
        """

        self.ratings = train_performance
        self.n_users, self.n_items = train_performance.shape
        self.n_factors = n_factors
        self.learning = learning
        if self.learning == 'sgd':
            self.n_samples, self.n_models = self.ratings.shape[0], \
                                            self.ratings.shape[1]
        self._v = verbose
        self.train_loss_ = [0]
        self.learning_rates_ = []
        self.scalar_ = None
        self.pca_ = None

    def get_train_dcg(self, user_vecs, item_vecs):
        # make sure it is non zero
        user_vecs[np.isnan(self.user_vecs)] = 0

        ndcg_s = []
        for w in range(self.ratings.shape[0]):
            ndcg_s.append(ndcg_score([self.ratings[w, :]],
                                     [np.dot(user_vecs[w, :], item_vecs.T)]))

        return np.mean(ndcg_s)

    def train(self, meta_features, n_iter=10,
              learning_rate=0.1, n_estimators=100, max_depth=10, max_rate=1.05,
              min_rate=0.1, discount=0.95, n_steps=10):
        """ Train model for n_iter iterations from scratch."""

        self.pca_ = PCA(n_components=self.n_factors)
        self.pca_.fit(meta_features)

        meta_features_pca = self.pca_.transform(meta_features)

        self.scalar_ = StandardScaler()
        self.scalar_.fit(meta_features_pca)

        meta_features_scaled = self.scalar_.transform(meta_features_pca)

        self.user_vecs = meta_features_scaled

        self.item_vecs = np.random.normal(scale=1. / self.n_factors,
                                          size=(self.n_items, self.n_factors))

        step_size = (max_rate - min_rate) / (n_steps - 1)
        lr_list = list(np.arange(min_rate, max_rate, step_size))
        lr_list.append(max_rate)
        lr_list_reverse = deepcopy(lr_list)
        lr_list_reverse.reverse()

        learning_rate_full = []
        for w in range(n_iter):
            learning_rate_full.extend(lr_list)
            learning_rate_full.extend(lr_list_reverse)

        self.learning_rate_ = min_rate
        self.learning_rates_.append(self.learning_rate_)

        ctr = 1
        np_ctr = 1
        while ctr <= n_iter:

            self.learning_rate_ = learning_rate_full[ctr - 1]
            self.learning_rates_.append(self.learning_rate_)

            self.regr_multirf = MultiOutputRegressor(RandomForestRegressor(
                n_estimators=n_estimators, max_depth=max_depth, n_jobs=4))

            # make sure it is non zero
            self.user_vecs[np.isnan(self.user_vecs)] = 0

            self.regr_multirf.fit(meta_features_scaled, self.user_vecs)

            train_indices = list(range(self.n_samples))
            np.random.shuffle(train_indices)
            # print(train_indices)

            for h in train_indices:

                uh = self.user_vecs[h, :].reshape(1, -1)
                # print(uh.shape)
                grads = []

                for i in range(self.n_models):
                    # outler loop
                    vi = self.item_vecs[i, :].reshape(-1, 1)
                    phis = []
                    rights = []
                    rights_v = []
                    # remove i from js 
                    js = list(range(self.n_models))
                    js.remove(i)

                    for j in js:
                        vj = self.item_vecs[j, :].reshape(-1, 1)
                        # temp_vt = np.exp(np.matmul(uh, (vj-vi)))
                        # temp_vt = np.ndarray.item(temp_vt)
                        temp_vt = sigmoid(
                            np.ndarray.item(np.matmul(uh, (vj - vi))), a=1)
                        temp_vt_derivative = sigmoid_derivate(
                            np.ndarray.item(np.matmul(uh, (vj - vi))), a=1)
                        # print(uh.re, (self.item_vecs[j,:]-self.item_vecs[i,:]).T.shape)
                        # print((self.item_vecs[j,:]-self.item_vecs[i,:]).reshape(-1, 1).shape)
                        # print(temp_vt.shape)
                        # assert (len(temp_vt)==1)
                        phis.append(temp_vt)
                        rights.append(temp_vt_derivative * (vj - vi))
                        rights_v.append(temp_vt_derivative * uh)
                    phi = np.sum(phis) + 1.5
                    rights = np.asarray(rights).reshape(self.n_models - 1,
                                                        self.n_factors)
                    rights_v = np.asarray(rights_v).reshape(self.n_models - 1,
                                                            self.n_factors)

                    # print(rights.shape, rights_v.shape)

                    right = np.sum(np.asarray(rights), axis=0)
                    right_v = np.sum(np.asarray(rights_v), axis=0)
                    # print(right, right_v)

                    # print(np.asarray(rights).shape, np.asarray(right).shape)
                    grad = (10 ** (self.ratings[h, i]) - 1) / (
                                phi * (np.log(phi)) ** 2) * right
                    grad_v = (10 ** (self.ratings[h, i]) - 1) / (
                                phi * (np.log(phi)) ** 2) * right_v

                    self.item_vecs[i, :] += self.learning_rate_ * grad_v

                    # print(h, i, grad.shape)
                    grads.append(grad)

                grads_uh = np.asarray(grads)
                grad_uh = np.sum(grads_uh, axis=0)

                self.user_vecs[h, :] -= self.learning_rate_ * grad_uh
                # print(self.learning_rate_)

            ctr += 1

        # disable unnecessary information
        self.ratings = None
        self.valid_ratings = None
        return self

    def predict(self, test_meta):
        test_meta = check_array(test_meta)
        #assert (test_meta.shape[1]==200)

        test_meta_scaled = self.pca_.transform(test_meta)
        # print('B', test_meta_scaled.shape)

        test_meta_scaled = self.scalar_.transform(test_meta_scaled)
        test_meta_scaled = self.regr_multirf.predict(test_meta_scaled)

        # predicted_scores = np.dot(test_k, self.item_vecs.T) + self.item_bias
        predicted_scores = np.dot(test_meta_scaled, self.item_vecs.T)
        # print(predicted_scores.shape)
        assert (predicted_scores.shape[0] == test_meta.shape[0])
        assert (predicted_scores.shape[1] == self.n_models)

        return predicted_scores
