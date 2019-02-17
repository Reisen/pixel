export default (...names: (string | boolean | undefined)[]) =>
    names.filter(name => !!name).join(' ');
