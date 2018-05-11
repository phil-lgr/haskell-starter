module Main where

import Exercice2Half(Exp (Evar), Exp (Ecall), Exp (Enum))

dosomething a = a

main = do
    let languageVar = Evar "X"
    print languageVar

    let languageConstant = Enum 123123
    print languageConstant

    let languageExpressionCall = Ecall languageConstant languageVar
    print languageExpressionCall