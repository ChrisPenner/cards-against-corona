#!/bin/bash

fd -e elm | entr elm make src/Main.elm --output=public/elm.js
