"use strict";
var happyBirthday = new Promise(function (pisces, feelings) {
    pisces('creation and intuition...');
})
    .then(function (heart) {
    console.log(heart);
    return 'Look inside with lucidity...';
})
    .then(function (past) {
    console.log(past);
    return 'Cherish what you have...';
})
    .then(function (present) {
    console.log(present);
    return 'Carry on what you love...';
})
    .then(function (future) {
    console.log(future);
    return '∞';
})
    .catch(function (sadness) {
    return 'Just let it go...';
});
//# sourceMappingURL=19.js.map