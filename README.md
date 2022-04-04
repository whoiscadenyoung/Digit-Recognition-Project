# Digit-Recognition-Project
Implementing a naive Bayesian classifier in Haskell

## Summary
This project required me to implement a naive Bayesian classifier for a set of scanned handwritten digit images. The goal is to guess which digit a handwritten image is supposed to be. A set (corpus) of 5000 training images was provided. Here is a summary of the project description as taken from the class project description:
* Instances are 28x28 black and white images.
* Labels are the digits 0 through 9.
* Since images are 28x28, there are 784 different pixels.
* There are two possible features for each pixel, one for each color (black=True or white=False). So theoretically there are 784*2=1568 features.

More information is provided on Bayesian classifiers here: https://sites.google.com/a/trinity.edu/csci2322-fl20/assignments/project-1-digit-classification/naive-bayesian-classifiers

## Files
Files of interest in the project:
* DigitRecognition.hs - This is the file I wrote code in
* TestInputs.hs - This file contains sample inputs for ghci to test functions
* Makefile - Used to build and test the project. 
  * make - builds the project.  The executable is output to classifier.
  * make clean - removes all generated files
* digitdata/ - Contains the images and labels for the data set. They are hard-coded
* singleimage - A single image, for testing purposes.
* trainingimages/traininglabels  - 5000 images and labels used to estimate probabilities.
* validationimages/validationlabels  - 1000 images and labels used to check the classifier.
* testimages/testlabels  - An alternate set of 1000 training images and labels, used by the --quick flag when testing your program.

## Code Execution
The project can be built using make. If the executable does not build, use `make setup` to install any missing Haskell packages. The executable and compiled source files can be removed using make clean.  The project compiles to classifier. To run grading tests on the project, use `./classifier --test`. To actually run the project on the images, use `./classifier -q -v 2 -n 20`. The following flags are supported: 

 `-h`  or  `--help`	Prints a help message and exits.
 `--test` Runs unit tests on your code.
 `-v n` Increase the verbosity of the output. 
 `-q` or `--quick` Speeds computation by using a smaller training set.
 `-c n` or `--count n`	Only test the first n images.
 `--timeout n` Time out computation after n minutes.
 `--ranking` If you've implemented rankImage, outputs the ranks of each digit instead of simply the best digit. 
 
 
