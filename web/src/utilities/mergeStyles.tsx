export default (...names: (string | boolean | undefined)[]) => {
    return names
        .filter(name => !!name)
        .join(' ')
};
