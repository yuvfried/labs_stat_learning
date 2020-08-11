
# From github/daviddalpiaz... 
# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706


#labels are stored in the y variables of each data frame
# can easily train many models using formula `y ~ .` syntax

# download data from http://yann.lecun.com/exdb/mnist/
if (!file.exists("train-images-idx3-ubyte")){
    
    download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
              "train-images-idx3-ubyte.gz")
    download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
              "train-labels-idx1-ubyte.gz")
    download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
              "t10k-images-idx3-ubyte.gz")
    download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
              "t10k-labels-idx1-ubyte.gz")

# gunzip the files
    R.utils::gunzip("train-images-idx3-ubyte.gz")
    R.utils::gunzip("train-labels-idx1-ubyte.gz")
    R.utils::gunzip("t10k-images-idx3-ubyte.gz")
    R.utils::gunzip("t10k-labels-idx1-ubyte.gz")
}

# helper function for visualization
show_digit = function(arr784, col = gray(128:1 / 128), ...) {
    image(matrix(as.matrix(arr784[-785]), nrow = 28)[, 28:1], col = col, ...)
}

# load image files
load_image_file = function(filename) {
    ret = list()
    f = file(filename, 'rb')
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
    close(f)
    data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
    f = file(filename, 'rb')
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
    close(f)
    y
}

# load images
train_data = load_image_file("train-images-idx3-ubyte")
test_data  = load_image_file("t10k-images-idx3-ubyte")

# load labels
train_data$y = as.factor(load_label_file("train-labels-idx1-ubyte"))
test_data$y  = as.factor(load_label_file("t10k-labels-idx1-ubyte"))

# view test image
show_digit(train_data[10000, ])
title(train_data$y[10000])

