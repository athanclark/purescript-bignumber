"use strict";

var BigNumber = require('bignumber.js');

exports.parseBigNumber = function parseBigNumber (Left,Right,s) {
    var x;
    try {
        x = new BigNumber(s);
    } catch (e) {
        return Left(e);
    }
    return x;
};


exports.configImpl = function configImpl (cfg) {
    BigNumber.config(cfg);
};
