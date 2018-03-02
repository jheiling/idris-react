#!/bin/bash

echo building package...
idris --build react.ipkg
idris --mkdoc react.ipkg

echo building example...
idris --build example.ipkg
