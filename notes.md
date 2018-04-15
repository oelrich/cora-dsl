# Notes regarding Cora DSL #

## CoraBasic ##

The assembly language of ***CoraSystem*** is expressed in the terms of the types ***DataAtomic*** and ***DataGroup*** and stored as Json data. All values are stored as strings.

### DataAtomic ###

Has a name, a value and an optional repeat ID.

### DataGroup ###

Has a name, an optional repeat ID, an optional ***attribute*** list and a list of children.

An ***attribute*** is a key-value-pair coded as '{"attribute_key": "attribute_value"}' in the Json.

The child list can be empty, but must be present in the Json.

Each child is either a ***DataAtomic*** or a ***DataGroup***. If a child has a repeat ID that ID must be unique with respect to its siblings.

## CoraSystem ##

The ***DataGroup*** name-field stores type information about the kind of information the ***DataGroup*** contains.

## CDSL ##

Wanted:

1. Functions to read and analyse the configuration files of Cora.

### Notes ###

***DataDivider*** selects what system the data belongs to. These are typically SystemOne/Alvin/Diva ...

Link to record type "system".

A parent is always an abstract type, not implementing.

Add rules to allow access when generating.
