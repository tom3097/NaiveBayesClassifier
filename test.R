
# przykladowy zbior labeli
y = c(1,2, 6, 4,2,5,7,5,3,5,4,7)

x = list(
  c(0,0,1,0,1),
  c(1,1,0,0,1),
  c(1,0,0,1,1),
  c(0,1,0,0,1),
  c(1,1,0,0,1),
  c(1,0,1,0,1),
  c(0,1,0,1,0),
  c(1,0,1,0,0),
  c(1,1,1,1,1),
  c(0,0,0,1,1),
  c(0,0,0,0,0),
  c(0,1,0,0,1)
)

uniq_y = unique(y)
classes = sort(uniq_y)
classes

denominator = length(y)

cls_prob = list()

cls_prob_v = c()

# liczymy prawdopodobienstwa kazdej z klas bez wygladzania
for (cls in classes) {
  print(paste("Class", cls))
  nominator = sum(y == cls)
  prob = nominator / denominator
  print(paste("Probability", prob))
  cls_prob[cls] = prob
  cls_prob_v[length(cls_prob_v)+1] = prob
}

print(cls_prob_v)

prob_table = matrix(cls_prob_v, ncol = length(classes), byrow = TRUE)
print(classes)

colnames(prob_table) = classes

names(cls_prob_v) = classes
print(cls_prob_v)

print("Class probabilities")

print(prob_table)

probabilities = list()

# liczymy prawdopodobienstwa warunkowe atrybyt a | label
for (cls in classes) {
  atr_prob = list()
  for (atr in sequence(5)) {
    print(paste("Class", cls, "Atrybut", atr))
    # lista przykladow ktore reprezentauja klase cls
    x_cls = x[y == cls]
    res_1 = sapply(x_cls, function(x) x[atr] == 1)
    res_0 = sapply(x_cls, function(x) x[atr] == 0)
    #print(res_0)
    #print(sum(res_0))
    #print(length(res_0))
    
    prob_0 = (sum(res_0)+1) / (length(res_0) + 2)
    prob_1 = (sum(res_1)+1) / (length(res_1) + 2)
    print( paste("probability of 0", prob_0))
    print( paste("Probability of 1", prob_1))
    
    atr_prob[atr] = list(c(prob_0, prob_1))
  }
  probabilities[cls] = list(atr_prob)
}

print(probabilities)

# proste predykcja
data = c(0,0,0,0,0)

cls_data = list()

for (cls in classes) {
  ilocz = 1.0
  cls_val = probabilities[[cls]]
  for (atr in sequence(5)) {
    index = data[atr] + 1
    ilocz = ilocz * cls_val[[atr]][index]
  }
  cls_data[cls] = ilocz
}


cls_data = sapply(classes, function(x) {
  cls_val = probabilities[[x]]
  ilocz = 1.0
  for (atr in sequence(5)) {
    index = data[atr] + 1
    ilocz = ilocz * cls_val[[atr]][index]
  }
  ilocz = ilocz * cls_prob[[x]]
  return(ilocz)
})

#dodawaj do listy koleje prawdopodobienstwa, ncol= liczba atrybutow, byrow=True
smoke <- matrix(c(51,43,22,92,28,21,68,22,9),ncol=3,byrow=TRUE)


print(cls_data)
pred_cls = which.max(cls_data)

print(paste("Predicted class", pred_cls))

library(e1071)

# tak najlepiej przygotowac dane
x <- data.frame("Atr1" = c(21,15), "Atr2" = c("John","Dora"))

# na wejscie bedzie przyjmowac dataframe x i dataframe y

xy.list <- setNames(split(x, seq(nrow(x))), rownames(x))
print(xy.list)

bernoulliBOW_x = data.frame("dog" = c(0,0,1,0,1,0,1,1,0,1), "cat" = c(1,1,0,1,1,0,0,0,1,0),
                          "head" = c(1,0,1,1,1,0,0,0,1,1), "neck" = c(1,0,0,0,1,0,0,1,1,0),
                          "bear" = c(1,0,1,1,0,1,0,0,0,0), "age" = c(0,0,0,0,1,1,1,1,0,0))

bernoulliBOW_y = data.frame("class" = c(1,2,2,3,1,3,2,2,3,1))

if (is.data.frame(bernoulliBOW_x)) {
  print("DUPA")
}
nrow(bernoulliBOW_x)

asd = function(x) {
  x = 1
}

# multinominal naive bayess
# 1. oblicz prawdopodobientstwa klas
# 2. jesli obiekt nalezy do klasy y=k, to sumuj ilosc wystapien
# 3. dodaj wygladzanie 


