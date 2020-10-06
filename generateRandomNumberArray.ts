const getRandomInt = (min: number, max: number) => {
    min = Math.ceil(min)
    max = Math.floor(max)
    return Math.floor(Math.random() * (max - min)) + min
}

export default (size: number, min: number, max: number) => {
    const array: number[] =  []
    if(size <= 0) return array;

    for (let i = 0; i < size; i++)
        array.push(getRandomInt(min, max))

    return array;
}