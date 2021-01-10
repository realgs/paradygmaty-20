/*

Tests for QUICK SORT

QuickSort time for 1000 ints:	1
ParallelQuickSort time for 1000 ints:	49

QuickSort time for 1000000 ints:	157
ParallelQuickSort time for 1000000 ints:	59

QuickSort time for 10000000 ints:	1459
ParallelQuickSort time for 10000000 ints:	575

------------------------------------------------
Tests for MERGE SORT

MergeSort time for 100 ints:	13
ParallelMergeSort time for 100 ints:	45

MergeSort time for 1000 ints:	41
ParallelMergeSort time for 1000 ints:	57

MergeSort time for 10000 ints:	1054
ParallelMergeSort time for 10000 ints:	598

------------------------------------------------
Tests for finding FIBONACCI NUMBERS

Find Fibonacci Number time - 25:	2
Find Parallel Fibonacci Number time - 25:	49
Find Fibonacci Number (tail rec) time - 25:	0

Find Fibonacci Number time - 35:	79
Find Parallel Fibonacci Number time - 35:	71
Find Fibonacci Number (tail rec) time - 35:	0

Find Fibonacci Number time - 40:	647
Find Parallel Fibonacci Number time - 40:	203
Find Fibonacci Number (tail rec) time - 40:	0

------------------------------------------------

We wszystkich testach widzimy, że stosowanie równoległości nie jest opłacalne dla operacji na małych ilościach danych.
Tak jak w przypadku quickSort, gdzie dla tablicy z 1000 elementów, program sekwencyjny wypada średnio ponad 40 razy lepiej.
Z kolei dla tablic powyżej 1 000 000 liczb możemy od zrównoleglonego rozwiązania oczekiwać około 3 razy lepszych wyników.
Dopiero po przekroczeniu pewnego progu zrównoleglanie staje się opłacalne.
Dodatkowo przykład szukania liczb fibonacciego pokazuje, że nawet dla dużych liczb,
równoległość nie zawsze jest rozwiązaniem optymalnym. Dzięki optymalizacjom istniejącym dla rekursji ogonowej jest ona nie do pobicia jeśli chodzi o czas obliczeń dla szukania liczb fibonacciego.

*/
