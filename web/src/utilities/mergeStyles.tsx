export default (...names: string[]) => {
    return names
        .filter(name => !!name)
        .join(' ')
};
