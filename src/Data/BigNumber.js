"use strict";

var BigNumber = require('bignumber.js');

exports.parseBigNumberImpl = function parseBigNumberImpl (Left,Right,s) {
    var x;
    try {
        x = new BigNumber(s);
    } catch (e) {
        return Left(e);
    }
    return Right(x);
};


exports.configImpl = function configImpl (cfg) {
    BigNumber.config(cfg);
};


exports.isBigNumber = function isBigNumber (x) {
    return BigNumber.isBigNumber(x);
};


exports.randomBigNumber = function randomBigNumber () {
    return BigNumber.random();
};


exports.absImpl = function absImpl (x) {
    return x.abs();
};


exports.compareBigNumberImpl = function compareBigNumberImpl (LT,EQ,GT,x,y) {
    var r = x.comparedTo(y);
    if (r === -1) {
        return LT;
    } else if (r === 0) {
        return EQ;
    } else if (r === 1) {
        return GT;
    }
};


exports.decimalPlacesImpl = function decimalPlacesImpl (x,y) {
    return x.decimalPlaces(y);
};

exports.divBigNumberImpl = function divBigNumberImpl (x,y) {
    return x.div(y);
};

exports.idivBigNumberImpl = function idivBigNumberImpl (x,y) {
    return x.idiv(y);
};

exports.powBigNumberImpl = function powBigNumberImpl (x,y) {
    return x.pow(y);
};

exports.intValue = function intValue (x) {
    return x.integerValue();
};

exports.eqBigNumberImpl = function eqBigNumberImpl (x,y) {
    return x.eq(y);
};

exports.isFinite = function isFinite (x) {
    return x.isFinite();
};

exports.gtBigNumberImpl = function gtBigNumberImpl (x,y) {
    return x.gt(y);
};

exports.gteBigNumberImpl = function gteBigNumberImpl (x,y) {
    return x.gte(y);
};

exports.isInteger = function isInteger (x) {
    return x.isInteger();
};

exports.ltBigNumberImpl = function ltBigNumberImpl (x,y) {
    return x.lt(y);
};

exports.lteBigNumberImpl = function lteBigNumberImpl (x,y) {
    return x.lte(y);
};

exports.isNaN = function isNaN (x) {
    return x.isNaN();
};

exports.isNegative = function isNegative (x) {
    return x.isNegative();
};

exports.isPositive = function isPositive (x) {
    return x.isPositive();
};

exports.isZero = function isZero (x) {
    return x.isZero();
};

exports.minusBigNumberImpl = function minusBigNumberImpl (x,y) {
    return x.minus(y);
};

exports.moduloBigNumberImpl = function moduloBigNumberImpl (x,y) {
    return x.modulo(y);
};

exports.timesBigNumberImpl = function timesBigNumberImpl (x,y) {
    return x.times(y);
};

exports.negateImpl = function negateImpl (x) {
    return x.negated();
};

exports.plusBigNumberImpl = function plusBigNumberImpl (x,y) {
    return x.plus(y);
};

exports.precisionImpl = function precisionImpl (x,y) {
    return x.precision(y);
};

exports.shiftedByImpl = function shiftedByImpl (x,y) {
    return x.shiftedBy(y);
};

exports.toNumber = function toNumber (x) {
    return x.toNumber();
};

exports.toString = function toString (x) {
    return x.toString();
};

exports.toExponential = function toExponential (x) {
    return x.toExponential();
};

exports.toFixed = function toFixed (x) {
    return x.toFixed();
};

exports.toFormat = function toFormat (x) {
    return x.toFormat();
};

exports.toFractionImpl = function toFractionImpl (x) {
    return x.toFraction();
};

exports.valueOf = function valueOf (x) {
    return x.valueOf();
};

exports.sqrt = function sqrt (x) {
    return x.sqrt();
};
