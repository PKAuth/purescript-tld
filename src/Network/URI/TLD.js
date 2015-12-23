"use strict";

// module Network.URI.TLD

// http://stackoverflow.com/questions/8498592/extract-root-domain-name-from-string/8498668#8498668
exports.extractDomainName = function( url) {
	var a = document.createElement('a');
	a.href = url;
	return a.hostname;
};

