const getRandomNumber = (min: number, max: number, useInts: boolean) => {
    min = Math.ceil(min)
    max = Math.floor(max)
    
    return useInts ? Math.floor(Math.random() * (max - min)) + min : parseFloat((Math.random() * (max - min) + min).toFixed(2))
}

export default (size: number, min: number, max: number, useInts: boolean) => {
    const array: number[] = []
    if(size <= 0) return array

    for (let i = 0; i < size; i++)
        array.push(getRandomNumber(min, max, useInts))

    return array
}