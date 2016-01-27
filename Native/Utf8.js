
// setup
Elm.Native = Elm.Native || {};
Elm.Native.Utf8 = Elm.Native.Utf8 || {};

// definition
Elm.Native.Utf8.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	if ('values' in Elm.Native.Utf8)
	{
		return Elm.Native.Utf8.values;
	}

	return (Elm.Native.Utf8.values = {
		encode: (function (str) {
      return window.unescape(encodeURIComponent(str));
    }),
		decode: (function (raw) {
      return decodeURIComponent(window.escape(raw));
    })
	});
};
