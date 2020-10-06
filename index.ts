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

const randomArray = generateRandomNumberArray(5, 0, 200)
const bubbleSortedArray = bubbleSort(randomArray)

console.log(bubbleSortedArray)