# Normalizing naming for "negative" attributes

* Proposal: TBD
* Author: [Erica Sadun](https://github.com/erica)
* Status: TBD
* Review manager: TBD

## Introduction

This proposal normalizes naming for "negative" attributes by adopting a rule 
that replaces camel-cased property names starting with `no` with adjectives 
starting with `non`. 

Swift-evolution thread:
[RFC: didset and willset](http://thread.gmane.org/gmane.comp.lang.swift.evolution/17534)

## Motivation

Converting `no`-prefixed attributes to `non` changes them into a single word that 
describes how they modify the syntax they decorate.

## Detailed Design

Upon adoption, Swift will rename:

* `noreturn` to `nonreturning`
* `noescape` to `nonescaping`

## Impact on Existing Code

This proposal requires migration support to rename keywords that use the old convention to
adopt the new convention. This is a simple substitution that should limit effect on code.

## Alternatives Considered

The core team may consider exceptions for terms of art. For example, "no return" indicates
"there is no return from this function", as in the "point of no return".