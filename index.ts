import generateRandomNumberArray from './generateRandomNumberArray';

const bubbleSort = (array: number[]): number[] => {
    array = array.slice()

    for(let i = 0; i < array.length; i++) {
        for(let j = 0; j < array.length - 1; j++) {
            if(array[j] > array[j + 1]) {
                let swap = array[j]
                array[j] = array[j + 1]
                array[j + 1] = swap
            }
        }
    }
    return array
}

const insertSort = (array: number[]): number[] => {
    array = array.slice()
 
    for(let i = 0; i < array.length; i++) {
        let j = i - 1
        let key = array[i]
        while((j > -1) && array[j] > key) {
            array[j + 1] = array[j]
            j--
        }
        array[j + 1] = key
    }
    return array
}

function testSorting(size: number, min: number, max: number, useInts: boolean) {
    const array = generateRandomNumberArray(size, min, max, useInts)
    const bubbleSortedArray = bubbleSort(array)
    const insertSortedArray = insertSort(array)

    console.log(bubbleSortedArray, insertSortedArray)
}

testSorting(10, 0, 200, true) // positive ints;
testSorting(10, -200, 200, true) // mixed ints;
testSorting(10, -200, -1, true) // negative ints;

testSorting(10, 0, 200, false) // positive doubles;
testSorting(10, -200, 200, false) // mixed doubles;
testSorting(10, -200, -1, false) // negative doubles;