library(R6)
library(ramify)

BernoulliNB <- R6Class("BernoulliNB",
  public = list(
    initialize = function() {
      private$cls_probabilities = list()
      private$cls_atr_probabilities = list()
    },
    fit = function(x, y) {
      stopifnot(is.data.frame(x))
      stopifnot(is.data.frame(y))
      stopifnot(ncol(y) == 1)
      stopifnot(nrow(x) == nrow(y))
      
      private$classes = unique(y[[1]])
      private$vocabulary_size = ncol(x)
      
      tmp = c()
      
      # calculates prior probabilities of classes
      for (cls in private$classes) {
        private$cls_probabilities[cls] = sum(y[[1]] == cls) / nrow(y)
        tmp[length(tmp)+1] = sum(y[[1]] == cls) / nrow(y)
      }
      
      print(length(tmp))
      print(length(private$classes))
      names(tmp) = private$classes
      
      print("Class probabilities")
      
      print(tmp)
      
      tmp2 = c()
      
      # calculates prior conditional probabilities of attributes
      for (cls in private$classes) {
        atr_probabilities = list()
        for (idx in sequence(ncol(x))) {
          cls_atr_count = sum(y[[1]] == cls & x[[idx]] == 1)
          cls_count = sum(y[[1]] == cls)
          # +1/+2 comes from Laplace smoothing
          probability = (cls_atr_count + 1)/(cls_count + 2)
          #atr_probabilities[idx] = probability
          tmp2[length(tmp2)+1] = probability
        }
        #private$cls_atr_probabilities[cls] = atr_probabilities
      }
      
      prob_table = matrix(tmp2, ncol = ncol(x), byrow = TRUE)
      colnames(prob_table) = colnames(x)
      #rownames(prob_table) = private$classes
      
      print("Conditional probabilities")
      
      print(prob_table)
      
      return(list(tmp, prob_table))
    },
    predict = function(x) {
      stopifnot(is.data.frame(x) && ncol(x) == self$vocabulary_size)
      
      cls_scores = sapply(self$classes, function(cls) {
        score = 1.0
        
      })
    }
  ),
  private = list(
    classes = NULL,
    vocabulary_size = NULL,
    cls_probabilities = NULL,
    cls_atr_probabilities = NULL
  )
)

bernoulliBOW_x = data.frame("dog" = c(0,0,1,0,1,0,1,1,0,1), "cat" = c(1,1,0,1,1,0,0,0,1,0),
                            "head" = c(1,0,1,1,1,0,0,0,1,1), "neck" = c(1,0,0,0,1,0,0,1,1,0),
                            "bear" = c(1,0,1,1,0,1,0,0,0,0), "age" = c(0,0,0,0,1,1,1,1,0,0))

bernoulliBOW_y = data.frame("class" = c(1,2,2,3,1,3,2,2,3,1))


b = BernoulliNB$new()
res = b$fit(bernoulliBOW_x, bernoulliBOW_y)
tmp = res[[1]]
print(tmp)
prob_table = res[[2]]
print(prob_table)

test = data.frame("dog" = c(0,1), "cat" = c(0,1), "head" = c(1,1), "neck" = c(0,0),
                  "bear" = c(1,1), "age" = c(1,0))
print(test)
m_test = as.matrix(test)
print(m_test)
m_prob = as.matrix(prob_table)
t_m_prob = t(m_prob)
print(t_m_prob)

a1 = m_test %*% t_m_prob
print(a1)
# wiersze = przyklady, kolumny - sumy prawdopodobienstw dla kazdej z klas
t_a1 = t(a1)
print(t_a1)
print(tmp)

are_col = matrix(test[[1]], ncol=1, byrow=TRUE)


# s = a %*%b + (1-a) %*% (1-b) - iloczyn macierze
final_res = t_a1 * tmp
print(final_res)
t_final_res = t(final_res)
print(t_final_res)

# Wyniki klasyfikacji:
class_results = argmax(t_final_res)
print(class_results)
