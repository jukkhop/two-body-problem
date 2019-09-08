// Generated by purs bundle 0.13.2
var PS = {};
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];                    
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  var compose = function (dict) {
      return dict.compose;
  };
  exports["compose"] = compose;
  exports["Semigroupoid"] = Semigroupoid;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];                
  var Category = function (Semigroupoid0, identity) {
      this.Semigroupoid0 = Semigroupoid0;
      this.identity = identity;
  };
  var identity = function (dict) {
      return dict.identity;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["Category"] = Category;
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];                
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS);
(function(exports) {
  "use strict";

  //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.length = function (xs) {
    return xs.length;
  };

  //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
        };
      };
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Category = $PS["Control.Category"];  
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var fromMaybe = function (a) {
      return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["maybe"] = maybe;
  exports["fromMaybe"] = fromMaybe;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Array"] = $PS["Data.Array"] || {};
  var exports = $PS["Data.Array"];
  var $foreign = $PS["Data.Array"];
  var Data_Maybe = $PS["Data.Maybe"];
  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["index"] = index;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS);
(function(exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];                
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS);
(function(exports) {
  "use strict";

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Data.Int"] = $PS["Data.Int"] || {};
  var exports = $PS["Data.Int"];
  var $foreign = $PS["Data.Int"];
  exports["toNumber"] = $foreign.toNumber;
})(PS);
(function(exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];                    
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applyEffect"] = applyEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  exports["log"] = $foreign.log;
})(PS);
(function(exports) {
  "use strict";

  exports.new = function (val) {
    return function () {
      return { value: val };
    };
  };

  exports.read = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports.write = function (val) {
    return function (ref) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Effect.Ref"] = $PS["Effect.Ref"] || {};
  var exports = $PS["Effect.Ref"];
  var $foreign = $PS["Effect.Ref"];
  exports["new"] = $foreign["new"];
  exports["read"] = $foreign.read;
  exports["write"] = $foreign.write;
})(PS);
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setCanvasWidth = function(canvas) {
      return function(width) {
          return function() {
              canvas.width = width;
          };
      };
  };

  exports.setCanvasHeight = function(canvas) {
      return function(height) {
          return function() {
              canvas.height = height;
          };
      };
  };

  exports.setFillStyle = function(ctx) {
      return function(style) {
          return function() {
              ctx.fillStyle = style;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
      };
  };

  exports.fill = function(ctx) {
      return function() {
          ctx.fill();
      };
  };

  exports.moveTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.moveTo(x, y);
              };
          };
      };
  };

  exports.arc = function(ctx) {
      return function(a) {
          return function() {
              ctx.arc(a.x, a.y, a.radius, a.start, a.end);
          };
      };
  };

  exports.clearRect = function(ctx) {
      return function(r) {
          return function() {
              ctx.clearRect(r.x, r.y, r.width, r.height);
          };
      };
  };

  exports.scale = function(ctx) {
      return function(t) {
          return function() {
              ctx.scale(t.scaleX, t.scaleY);
          };
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Graphics.Canvas"] = $PS["Graphics.Canvas"] || {};
  var exports = $PS["Graphics.Canvas"];
  var $foreign = $PS["Graphics.Canvas"];
  var Data_Maybe = $PS["Data.Maybe"];
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["setCanvasWidth"] = $foreign.setCanvasWidth;
  exports["setCanvasHeight"] = $foreign.setCanvasHeight;
  exports["setFillStyle"] = $foreign.setFillStyle;
  exports["beginPath"] = $foreign.beginPath;
  exports["fill"] = $foreign.fill;
  exports["moveTo"] = $foreign.moveTo;
  exports["arc"] = $foreign.arc;
  exports["clearRect"] = $foreign.clearRect;
  exports["scale"] = $foreign.scale;
})(PS);
(function(exports) {
  "use strict";          

  exports.floor = Math.floor;

  exports.pow = function (n) {
    return function (p) {
      return Math.pow(n, p);
    };
  };                     

  exports.sqrt = Math.sqrt;    

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["floor"] = $foreign.floor;
  exports["pow"] = $foreign.pow;
  exports["sqrt"] = $foreign.sqrt;
  exports["pi"] = $foreign.pi;
})(PS);
(function(exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Web.HTML"] = $PS["Web.HTML"] || {};
  var exports = $PS["Web.HTML"];
  var $foreign = $PS["Web.HTML"];
  exports["window"] = $foreign.window;
})(PS);
(function(exports) {
  "use strict";

  exports.innerWidth = function (window) {
    return function () {
      return window.innerWidth;
    };
  };

  exports.innerHeight = function (window) {
    return function () {
      return window.innerHeight;
    };
  };

  exports._requestAnimationFrame = function(fn) {
    return function(window) {
      return function() {
        return window.requestAnimationFrame(fn);
      };
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});
(function($PS) {
  // Generated by purs version 0.13.2
  "use strict";
  $PS["Web.HTML.Window"] = $PS["Web.HTML.Window"] || {};
  var exports = $PS["Web.HTML.Window"];
  var $foreign = $PS["Web.HTML.Window"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect = $PS["Effect"];
  var RequestAnimationFrameId = function (x) {
      return x;
  };
  var requestAnimationFrame = function (fn) {
      var $33 = Data_Functor.map(Effect.functorEffect)(RequestAnimationFrameId);
      var $34 = $foreign["_requestAnimationFrame"](fn);
      return function ($35) {
          return $33($34($35));
      };
  };
  exports["requestAnimationFrame"] = requestAnimationFrame;
  exports["innerWidth"] = $foreign.innerWidth;
  exports["innerHeight"] = $foreign.innerHeight;
})(PS);
(function($PS) {
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Data_Array = $PS["Data.Array"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect = $PS["Effect"];
  var Effect_Console = $PS["Effect.Console"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Graphics_Canvas = $PS["Graphics.Canvas"];
  var $$Math = $PS["Math"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];                
  var verletVel = function (vel) {
      return function (dt) {
          return function (currAccel) {
              return function (nextAccel) {
                  return vel + 0.5 * (nextAccel + currAccel) * dt;
              };
          };
      };
  };
  var verletPos = function (pos) {
      return function (vel) {
          return function (dt) {
              return function (currAccel) {
                  return pos + vel * dt + 0.5 * currAccel * $$Math.pow(dt)(2.0);
              };
          };
      };
  };
  var initialVelocity = function (ratio) {
      return function (ecc) {
          return $$Math.sqrt(1.0 + ratio) * (1.0 + ecc);
      };
  };
  var consts = {
      eccentricity: 0.7,
      gravity: 10.0,
      massRatio: 1.0,
      timeStep: 5.0e-3
  };
  var config = {
      bodyColor: "#ffffff",
      bodyRadius: 7.0,
      pixelRatio: 2.0,
      scale: 350.0
  };
  var scaleCanvas = function (wind) {
      return function (canv) {
          return function (ctx) {
              return function __do() {
                  var v = Web_HTML_Window.innerWidth(wind)();
                  var v1 = Web_HTML_Window.innerHeight(wind)();
                  Graphics_Canvas.setCanvasWidth(canv)(config.pixelRatio * Data_Int.toNumber(v))();
                  Graphics_Canvas.setCanvasHeight(canv)(config.pixelRatio * Data_Int.toNumber(v1))();
                  return Graphics_Canvas.scale(ctx)({
                      scaleX: config.pixelRatio,
                      scaleY: config.pixelRatio
                  })();
              };
          };
      };
  };
  var translatePos = function (v) {
      return function (v1) {
          var middleY = $$Math.floor(v.height / 2.0);
          var middleX = $$Math.floor(v.width / 2.0);
          var centerY = v1.y * config.scale + middleY;
          var centerX = v1.x * config.scale + middleX;
          return {
              x: centerX,
              y: centerY
          };
      };
  };
  var render = function (dims) {
      return function (ctx) {
          return function (stateRef) {
              return function __do() {
                  var v = Effect_Ref.read(stateRef)();
                  var end = 2.0 * $$Math.pi;
                  var def = {
                      x: 0.0,
                      y: 0.0
                  };
                  var pos1 = Data_Maybe.fromMaybe(def)(Data_Array.index(v.positions)(0));
                  var tpos1 = translatePos(dims)(pos1);
                  var pos2 = Data_Maybe.fromMaybe(def)(Data_Array.index(v.positions)(1));
                  var tpos2 = translatePos(dims)(pos2);
                  Graphics_Canvas.clearRect(ctx)({
                      width: dims.width,
                      height: dims.height,
                      x: 0.0,
                      y: 0.0
                  })();
                  Graphics_Canvas.beginPath(ctx)();
                  Graphics_Canvas.moveTo(ctx)(tpos1.x + config.bodyRadius)(tpos1.y)();
                  Graphics_Canvas.arc(ctx)({
                      x: tpos1.x,
                      y: tpos1.y,
                      radius: config.bodyRadius,
                      start: 0.0,
                      end: end
                  })();
                  Graphics_Canvas.moveTo(ctx)(tpos2.x + config.bodyRadius)(tpos2.y)();
                  Graphics_Canvas.arc(ctx)({
                      x: tpos2.x,
                      y: tpos2.y,
                      radius: config.bodyRadius,
                      start: 0.0,
                      end: end
                  })();
                  Graphics_Canvas.setFillStyle(ctx)(config.bodyColor)();
                  return Graphics_Canvas.fill(ctx)();
              };
          };
      };
  };
  var accel = function (radius) {
      return function (unitX) {
          return function (unitY) {
              var scalar = -(consts.gravity * consts.massRatio) / $$Math.pow(radius)(2.0);
              var accelY = scalar * unitY;
              var accelX = scalar * unitX;
              return {
                  x: accelX,
                  y: accelY
              };
          };
      };
  };
  var update = function (wind) {
      return function (dims) {
          return function (ctx) {
              return function (stateRef) {
                  return function __do() {
                      var v = Effect_Ref.read(stateRef)();
                      var radius = $$Math.sqrt($$Math.pow(v.x)(2.0) + $$Math.pow(v.y)(2.0));
                      var currAccel = accel(radius)(v.x / radius)(v.y / radius);
                      var newX = verletPos(v.x)(v.vx)(consts.timeStep)(currAccel.x);
                      var newY = verletPos(v.y)(v.vy)(consts.timeStep)(currAccel.y);
                      var newRadius = $$Math.sqrt($$Math.pow(newX)(2.0) + $$Math.pow(newY)(2.0));
                      var newAccel = accel(newRadius)(newX / newRadius)(newY / newRadius);
                      var newVx = verletVel(v.vx)(consts.timeStep)(currAccel.x)(newAccel.x);
                      var newVy = verletVel(v.vy)(consts.timeStep)(currAccel.y)(newAccel.y);
                      var a2 = v.masses.m1 / v.masses.total;
                      var a1 = v.masses.m2 / v.masses.total;
                      var newState = {
                          x: newX,
                          y: newY,
                          vx: newVx,
                          vy: newVy,
                          masses: v.masses,
                          positions: [ {
                              x: a1 * newX,
                              y: a1 * newY
                          }, {
                              x: -a2 * newX,
                              y: -a2 * newY
                          } ]
                      };
                      Effect_Ref.write(newState)(stateRef)();
                      render(dims)(ctx)(stateRef)();
                      return Data_Functor["void"](Effect.functorEffect)(Web_HTML_Window.requestAnimationFrame(update(wind)(dims)(ctx)(stateRef))(wind))();
                  };
              };
          };
      };
  };
  var main = function __do() {
      var v = Graphics_Canvas.getCanvasElementById("canvas")();
      if (v instanceof Data_Maybe.Nothing) {
          return Effect_Console.log("Canvas not found")();
      };
      if (v instanceof Data_Maybe.Just) {
          var v1 = Graphics_Canvas.getContext2D(v.value0)();
          var v2 = Web_HTML.window();
          var v3 = Web_HTML_Window.innerWidth(v2)();
          var v4 = Web_HTML_Window.innerHeight(v2)();
          scaleCanvas(v2)(v.value0)(v1)();
          var state = {
              x: 1.0,
              y: 0.0,
              vx: 0.0,
              vy: initialVelocity(1.0)(consts.eccentricity),
              masses: {
                  m1: 1.0,
                  m2: consts.massRatio,
                  ratio: consts.massRatio,
                  total: 1.0 + consts.massRatio
              },
              positions: [ {
                  x: 0.0,
                  y: 0.0
              }, {
                  x: 0.0,
                  y: 0.0
              } ]
          };
          var dims = {
              width: Data_Int.toNumber(v3),
              height: Data_Int.toNumber(v4)
          };
          var v5 = Effect_Ref["new"](state)();
          return Data_Functor["void"](Effect.functorEffect)(Web_HTML_Window.requestAnimationFrame(update(v2)(dims)(v1)(v5))(v2))();
      };
      throw new Error("Failed pattern match at Main (line 67, column 3 - line 99, column 72): " + [ v.constructor.name ]);
  };
  exports["consts"] = consts;
  exports["config"] = config;
  exports["main"] = main;
  exports["scaleCanvas"] = scaleCanvas;
  exports["update"] = update;
  exports["render"] = render;
  exports["accel"] = accel;
  exports["verletPos"] = verletPos;
  exports["verletVel"] = verletVel;
  exports["initialVelocity"] = initialVelocity;
  exports["translatePos"] = translatePos;
})(PS);
PS["Main"].main();