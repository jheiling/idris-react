#!/bin/bash

echo cleaning package...
idris --clean react.ipkg

echo cleaning example...
idris --clean example.ipkg
