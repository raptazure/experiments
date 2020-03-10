const happyBirthday = new Promise((pisces, feelings) => {
    pisces('creation and intuition...');
})
    .then(heart => {
    console.log(heart);
    return 'Look inside with lucidity...';
})
    .then(past => {
    console.log(past);
    return 'Cherish what you have...';
})
    .then(present => {
    console.log(present);
    return 'Carry on what you love...';
})
    .then(future => {
    console.log(future);
    return '∞';
})
    .catch(sadness => {
    return 'Just let it go...';
});
//# sourceMappingURL=19.js.map