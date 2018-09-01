"use strict";

var BigNumber = require('bignumber.js');

exports.parseBigNumberImpl = function parseBigNumberImpl (Left,Right,s) {
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


exports.isBigNumber = function isBigNumber (x) {
    return BigNumber.isBigNumber(x);
};


exports.maxBigNumberImpl = function maxBigNumberImpl (x,y) {
    return BigNumber.maximum(x,y);
};


exports.minBigNumberImpl = function minBigNumberImpl (x,y) {
    return BigNumber.minimum(x,y);
};


exports.randomBigNumber = function randomBigNumber () {
    return BigNumber.random();
};
