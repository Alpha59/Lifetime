# Lifetime
No Desc Avail
## Synopsis
This project is just a tool that I have created to manage my own budget. The program is designed to be an R program, assisted by an excel file (A blank copy of which has been provided here). Mostly it just accounts for income, and calculates what larger purchases can be afforded based on daily spending habits.

## Motivation
This project came from a desire to plan, and the seemingly sparse number of projects that account for all of the variables which I wished to include. I wanted a program that could account for buying and selling of large assets, daily expensses, retirement, and accounted for inflation and changing values. Although all of this is obviously possible in pure excel, I wanted to build something that would just use a configuration file (and excel works well as a financial config file) but still had viewable, trackable, changeable logic to work with.

## Installation
Installing is simple if you already have R and excel installed. Otherwise, this could be problematic... So start by installing those 2 programs, and then run this script by just running:

```
Rscript budget.r
```

or, run `chmod +x budget.r` and you can run the program with the simplier command `./budget.r`.

## API Reference
Right now there is no API for the program, although I may add some parameters later if I find myself changing the config file to run tests.

## Tests
Testing the program right now is a manual checking process, although I would like to test each of the functions with unit tests in the future.

## Contributors
If you have any suggestions or changes- please just generate a PR from a feature branch, and give a good description of what the change is.

