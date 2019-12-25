import reductions._

reductions.ParallelParenthesesBalancing.balance(Array('(', ')'))

reductions.ParallelParenthesesBalancing.parBalance(Array('(', '('), 3)

reductions.ParallelParenthesesBalancing.parBalance(Array('(', ')'), 3)

reductions.ParallelParenthesesBalancing.parBalance(Array('(', '(', ')', ')'), 2)
reductions.ParallelParenthesesBalancing.parBalance(Array('(', '(', '(', ')', ')'), 2)



reductions.ParallelParenthesesBalancing.parBalance(Array(')', ')', '(', '(', '(', '(', ')', ')'), 4)



reductions.ParallelParenthesesBalancing.parBalance(Array(')', ')', '(', '(', '(', '(', ')', ')'), 4)


reductions.ParallelParenthesesBalancing.parBalance(Array('(', '(', ')', ')', ')', ')'), 2)

reductions.ParallelParenthesesBalancing.parBalance("(one (two (three (four (five (six (seven (eight (nine (ten))))))))))".toCharArray, 8)



val length = 10000000
val chars = new Array[Char](length)
val threshold = 10000

//reductions.ParallelParenthesesBalancing.balance(chars)
reductions.ParallelParenthesesBalancing.parBalance(chars, 10000)
