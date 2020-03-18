"use strict";
var getUserName = function (user) { return user.name; };
function CalculateAreas(config) {
    var square = 100;
    if (config.width) {
        square = config.width * config.width;
    }
    return { area: square };
}
//# sourceMappingURL=interface.js.map