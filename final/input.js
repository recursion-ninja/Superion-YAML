  // Superion
  function shouldBe(actual, expected) {
      if (actual !== expected)
          throw new Error('bad value: ' + actual);
  }
  var f = new Function(`obj`, `return 0 ${"+ obj.i".repeat(1000)}`);
  shouldBe(f({ i: 42 }), 42000);
  
  // Gramatron
  substr Int32Array;
  from WebAssembly;
  sqrt WebAssembly.RuntimeError.__proto__.LOG2E
      .__parent__.__proto__.__count__  ;
  d.LN10.exports.SQRT2.global.MAX_VALUE.replace.SQRT1_2
      .unscopables.buffer.hasInstance.__proto__.ignoreCase.__proto__
      .unicode.BYTES_PER_ELEMENT.byteLength.lineNumber.prototype.format  ;
  g Error.toStringTag.length.match.BYTES_PER_ELEMENT  ;
  seal Object;
  findIndex Intl.Collator;