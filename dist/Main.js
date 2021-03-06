Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Main",
   $Debug = Elm.Debug.make(_elm),
   $GameDisplay = Elm.GameDisplay.make(_elm),
   $GameInput = Elm.GameInput.make(_elm),
   $GameModel = Elm.GameModel.make(_elm),
   $GameUpdates = Elm.GameUpdates.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm),
   $Window = Elm.Window.make(_elm);
   var delta = A2($Signal._op["<~"],
   $Time.inSeconds,
   $Time.fps(30));
   var input = A2($Signal._op["<~"],
   $Debug.watch("input"),
   A2($Signal.sampleOn,
   delta,
   A3($Signal.lift2,
   $GameInput.Input,
   delta,
   $GameInput.userInput)));
   var gameState = A3($Signal.foldp,
   $GameUpdates.stepGame,
   $GameModel.defaultGame,
   input);
   var main = A3($Signal.lift2,
   $GameDisplay.display,
   $Window.dimensions,
   gameState);
   _elm.Main.values = {_op: _op
                      ,delta: delta
                      ,input: input
                      ,gameState: gameState
                      ,main: main};
   return _elm.Main.values;
};Elm.GameDisplay = Elm.GameDisplay || {};
Elm.GameDisplay.make = function (_elm) {
   "use strict";
   _elm.GameDisplay = _elm.GameDisplay || {};
   if (_elm.GameDisplay.values)
   return _elm.GameDisplay.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameDisplay",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $GameDraw = Elm.GameDraw.make(_elm),
   $GameModel = Elm.GameModel.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $String = Elm.String.make(_elm),
   $Text = Elm.Text.make(_elm);
   var msg = "SPACE to start, AD to rotate, &uarr;&darr;&larr;&rarr; to move";
   var title = "BUGS ARE HELL";
   var txt = function (f) {
      return function ($) {
         return $Text.centered(f($Text.color($GameDraw.playerColour2)($Text.toText($))));
      };
   };
   var displayScore = function (score) {
      return $Graphics$Collage.move({ctor: "_Tuple2"
                                    ,_0: 0
                                    ,_1: $GameModel.gameHeight / 2})($Graphics$Collage.toForm(A2(txt,
      $Text.height(40),
      $String.show(score))));
   };
   var displayStatus = function (state) {
      return _U.eq(state,
      $GameModel.Pause) ? _L.fromArray([$Graphics$Collage.move({ctor: "_Tuple2"
                                                               ,_0: 0
                                                               ,_1: -80})($Graphics$Collage.toForm(A2(txt,
                                       $Text.height(25),
                                       $Basics.identity(msg))))
                                       ,$Graphics$Collage.move({ctor: "_Tuple2"
                                                               ,_0: 0
                                                               ,_1: 80})($Graphics$Collage.toForm(A2(txt,
                                       $Text.height(40),
                                       $Basics.identity(title))))]) : _L.fromArray([]);
   };
   var fixWeirdAngles = F2(function (_v0,
   shape) {
      return function () {
         return _U.cmp(_v0.spr.sides,
         2) > 0 ? A2($Graphics$Collage.rotate,
         _v0.rot.angle + $Basics.pi / $Basics.toFloat(_v0.spr.sides),
         shape) : A2($Graphics$Collage.rotate,
         _v0.rot.angle + $Basics.pi / 2,
         shape);
      }();
   });
   var displayActor = function (_v2) {
      return function () {
         return fixWeirdAngles(_v2)($Graphics$Collage.move({ctor: "_Tuple2"
                                                           ,_0: _v2.pos.x
                                                           ,_1: _v2.pos.y})(_v2.spr.shape));
      }();
   };
   var displayActors = function (actors) {
      return $List.map(function (actor) {
         return displayActor(actor);
      })(actors);
   };
   var display = F2(function (_v4,
   _v5) {
      return function () {
         return function () {
            switch (_v4.ctor)
            {case "_Tuple2":
               return function () {
                    var displayedActors = _L.append(_L.fromArray([$Graphics$Collage.filled($Color.black)(A2($Graphics$Collage.rect,
                    $Basics.toFloat(_v4._0),
                    $Basics.toFloat(_v4._1)))]),
                    _L.append(_L.fromArray([displayScore(_v5.score)]),
                    _L.append(displayStatus(_v5.state),
                    _L.append(displayActors(_v5.enemies),
                    _L.append(displayActors(_v5.playerBullets),
                    _L.append(displayActors(_v5.enemyBullets),
                    _L.fromArray([displayActor(_v5.player)])))))));
                    return A3($Graphics$Element.container,
                    _v4._0,
                    _v4._1,
                    $Graphics$Element.middle)(A3($Graphics$Collage.collage,
                    _v4._0,
                    _v4._1,
                    displayedActors));
                 }();}
            _E.Case($moduleName,
            "between lines 41 and 51");
         }();
      }();
   });
   _elm.GameDisplay.values = {_op: _op
                             ,fixWeirdAngles: fixWeirdAngles
                             ,displayActor: displayActor
                             ,displayActors: displayActors
                             ,txt: txt
                             ,displayScore: displayScore
                             ,title: title
                             ,msg: msg
                             ,displayStatus: displayStatus
                             ,display: display};
   return _elm.GameDisplay.values;
};Elm.GameUpdates = Elm.GameUpdates || {};
Elm.GameUpdates.make = function (_elm) {
   "use strict";
   _elm.GameUpdates = _elm.GameUpdates || {};
   if (_elm.GameUpdates.values)
   return _elm.GameUpdates.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameUpdates",
   $Basics = Elm.Basics.make(_elm),
   $GameDraw = Elm.GameDraw.make(_elm),
   $GameEnemies = Elm.GameEnemies.make(_elm),
   $GameInput = Elm.GameInput.make(_elm),
   $GameModel = Elm.GameModel.make(_elm),
   $List = Elm.List.make(_elm),
   $Pseudorandom = Elm.Pseudorandom.make(_elm),
   $Time = Elm.Time.make(_elm);
   var stepUI = F3(function (t,
   player,
   ui) {
      return ui;
   });
   var stepSpawnCooldown = F3(function (t,
   score,
   spawnCooldown) {
      return _U.cmp(spawnCooldown,
      0) < 1 ? 1 : spawnCooldown - t;
   });
   var addScoreForKill = F2(function (enemy,
   acc) {
      return enemy.kill ? acc + 100 : acc;
   });
   var stepScore = F3(function (t,
   enemies,
   score) {
      return A3($List.foldl,
      addScoreForKill,
      score,
      enemies);
   });
   var fireEnemyBullets = F2(function (t,
   _v0) {
      return function () {
         return _U.eq(_v0.gun.timeSince,
         0) ? $GameModel.createEnemyBullets(_v0) : _L.fromArray([]);
      }();
   });
   var spawnEnemyBatch = F4(function (seeds,
   spawnCooldown,
   player,
   enemies) {
      return function () {
         var formation = A2($GameEnemies.squareFormation,
         3,
         40);
         var dir = {_: {}
                   ,x: $Pseudorandom.get(seeds.c)($Pseudorandom.$float) - $Pseudorandom.get(seeds.a)($Pseudorandom.$float)
                   ,y: $Pseudorandom.get(seeds.d)($Pseudorandom.$float) - $Pseudorandom.get(seeds.b)($Pseudorandom.$float)};
         var pos = {_: {}
                   ,x: player.pos.x + 100 * $Basics.toFloat($Pseudorandom.get(seeds.a)($Pseudorandom.range({ctor: "_Tuple2"
                                                                                                           ,_0: -3
                                                                                                           ,_1: 3})))
                   ,y: player.pos.y + 100 * $Basics.toFloat($Pseudorandom.get(seeds.b)($Pseudorandom.range({ctor: "_Tuple2"
                                                                                                           ,_0: -3
                                                                                                           ,_1: 3})))};
         return _U.cmp(spawnCooldown,
         0) < 1 ? _L.append(A3($GameModel.createEnemyBatch,
         formation,
         pos,
         dir),
         enemies) : enemies;
      }();
   });
   var isActorKilled = function (actor) {
      return actor.kill;
   };
   var stepPlayerSpriteLivesSides = function (player) {
      return _U.eq(player.lives + 2,
      player.spr.sides) ? player : _U.replace([["spr"
                                               ,$GameDraw.playerSides(player.lives + 2)]],
      player);
   };
   var reloadGun = F2(function (t,
   gun) {
      return function () {
         var timeSince$ = gun.timeSince + t;
         return _U.replace([["timeSince"
                            ,timeSince$]],
         gun);
      }();
   });
   var fireGun = function (gun) {
      return _U.replace([["timeSince"
                         ,0]],
      gun);
   };
   var stepGun = F2(function (t,
   _v2) {
      return function () {
         return _U.cmp(_v2.gun.timeSince,
         1 / _v2.gun.fireRate) > 0 ? _U.replace([["gun"
                                                 ,fireGun(_v2.gun)]],
         _v2) : _U.replace([["gun"
                            ,A2(reloadGun,t,_v2.gun)]],
         _v2);
      }();
   });
   var stepPlayerGun = F3(function (t,
   fire,
   _v4) {
      return function () {
         return function () {
            var gun$ = _U.replace([["firing"
                                   ,fire]],
            _v4.gun);
            return fire ? A2(stepGun,
            t,
            _U.replace([["gun",gun$]],
            _v4)) : _U.replace([["gun"
                                ,gun$]],
            _v4);
         }();
      }();
   });
   var stepActorLivesCooldown = F2(function (t,
   _v6) {
      return function () {
         return _U.cmp(_v6.loseLifeCooldown,
         0) < 1 ? _U.replace([["loseLifeCooldown"
                              ,0]],
         _v6) : _U.replace([["loseLifeCooldown"
                            ,_v6.loseLifeCooldown - t]],
         _v6);
      }();
   });
   var accelerate = F5(function (t,
   accel,
   speed,
   dir,
   vel) {
      return A3($Basics.clamp,
      0 - speed,
      speed,
      vel + accel * dir);
   });
   var movePos = F3(function (t,
   pos,
   vel) {
      return {_: {}
             ,x: A3($Basics.clamp,
             0 - $GameModel.halfWidth,
             $GameModel.halfWidth,
             pos.x + vel.vx * t)
             ,y: A3($Basics.clamp,
             0 - $GameModel.halfHeight,
             $GameModel.halfHeight,
             pos.y + vel.vy * t)};
   });
   var actorPosition = F2(function (t,
   actor) {
      return A3(movePos,
      t,
      actor.pos,
      actor.vel);
   });
   var unitVector = function (pos) {
      return function () {
         var m = $Basics.sqrt(Math.pow(pos.x,
         2) + Math.pow(pos.y,2));
         return _U.replace([["x"
                            ,pos.x / m]
                           ,["y",pos.y / m]],
         pos);
      }();
   };
   var unitVectorTwoPoints = F2(function (pos,
   pos$) {
      return function () {
         var v = {_: {}
                 ,x: pos.x - pos$.x
                 ,y: pos.y - pos$.y};
         return unitVector(v);
      }();
   });
   var dist = F2(function (pos,
   pos$) {
      return $Basics.sqrt(Math.pow(pos.x - pos$.x,
      2) + Math.pow(pos.y - pos$.y,
      2));
   });
   var collides = F2(function (actor,
   actor$) {
      return _U.cmp(A2(dist,
      actor.pos,
      actor$.pos),
      $Basics.toFloat(actor.spr.size) + $Basics.toFloat(actor$.spr.size)) < 0 ? true : false;
   });
   var isFalse = function (val) {
      return val ? false : true;
   };
   var isTrue = function (val) {
      return val ? true : false;
   };
   var collider = F2(function (collideActors,
   actor) {
      return function () {
         var len = $List.length(A2($List.filter,
         isTrue,
         A2($List.map,
         function (ca) {
            return A2(collides,ca,actor);
         },
         collideActors)));
         return _U.cmp(len,
         0) > 0 ? true : false;
      }();
   });
   var stepActorLives = F3(function (t,
   bullets,
   actor) {
      return function () {
         var bulletsCollider = collider(bullets);
         return bulletsCollider(actor) ? _U.replace([["lives"
                                                     ,actor.lives - 1]
                                                    ,["loseLifeCooldown"
                                                     ,actor.loseLifeCooldownMax]],
         actor) : actor;
      }();
   });
   var stepActorLivesWithCooldown = F3(function (t,
   bullets,
   actor) {
      return _U.cmp(actor.loseLifeCooldown,
      0) < 1 ? A3(stepActorLives,
      t,
      bullets,
      actor) : actor;
   });
   var killEnemy = F2(function (playerBullets,
   enemy) {
      return collider(playerBullets)(enemy) ? _U.replace([["kill"
                                                          ,true]],
      enemy) : enemy;
   });
   var outside = F3(function (value,
   compare,
   offset) {
      return _U.cmp(value,
      compare - offset) < 1 || _U.cmp(value,
      compare + offset) > -1;
   });
   var avoidEdgesActor = F2(function (t,
   _v8) {
      return function () {
         return function () {
            var flipPosY = _U.replace([["y"
                                       ,0]],
            _v8.pos);
            var flipPosX = _U.replace([["x"
                                       ,0]],
            _v8.pos);
            var flipDirY = _U.replace([["y"
                                       ,0 - _v8.dir.y]],
            _v8.dir);
            var flipDirX = _U.replace([["x"
                                       ,0 - _v8.dir.x]],
            _v8.dir);
            return A3(outside,
            _v8.pos.x,
            0,
            $GameModel.halfWidth - 10) ? _U.replace([["pos"
                                                     ,flipPosX]],
            _v8) : A3(outside,
            _v8.pos.y,
            0,
            $GameModel.halfHeight - 10) ? _U.replace([["pos"
                                                      ,flipPosY]],
            _v8) : _v8;
         }();
      }();
   });
   var near = F3(function (value,
   compare,
   offset) {
      return _U.cmp(value,
      compare - offset) > -1 && _U.cmp(value,
      compare + offset) < 1;
   });
   var onCanvas = function (_v10) {
      return function () {
         return A3(near,
         _v10.pos.x,
         0,
         $GameModel.halfWidth - 1) && A3(near,
         _v10.pos.y,
         0,
         $GameModel.halfHeight - 1);
      }();
   };
   var tendToClose = F4(function (current,
   target,
   step,
   give) {
      return _U.cmp(current,
      target - give) > 0 ? current - step : _U.cmp(current,
      target + give) < 0 ? current + step : target;
   });
   var tendTo = F3(function (current,
   target,
   step) {
      return _U.cmp(current,
      target) > 0 ? current - step : _U.cmp(current,
      target) < 0 ? current + step : target;
   });
   var actorVelocity = F3(function (t,
   dir,
   _v12) {
      return function () {
         return {_: {}
                ,vx: _U.eq(dir.x,0) ? A3(tendTo,
                _v12.vel.vx,
                0,
                _v12.deccel) : A5(accelerate,
                t,
                _v12.accel,
                _v12.speed,
                dir.x,
                _v12.vel.vx)
                ,vy: _U.eq(dir.y,0) ? A3(tendTo,
                _v12.vel.vy,
                0,
                _v12.deccel) : A5(accelerate,
                t,
                _v12.accel,
                _v12.speed,
                dir.y,
                _v12.vel.vy)};
      }();
   });
   var moveActor = F3(function (t,
   dir,
   actor) {
      return function () {
         var vel$ = A3(actorVelocity,
         t,
         dir,
         actor);
         var pos$ = A2(actorPosition,
         t,
         actor);
         return _U.replace([["pos",pos$]
                           ,["vel",vel$]],
         actor);
      }();
   });
   var actorRotationalVelocity = F3(function (t,
   dir,
   _v14) {
      return function () {
         return _U.eq(dir.x,
         0) ? A3(tendTo,
         _v14.rot.vel,
         0,
         _v14.rot.deccel) : A5(accelerate,
         t,
         _v14.rot.accel,
         _v14.rot.speed,
         0 - dir.x,
         _v14.rot.vel);
      }();
   });
   var actorAngle = F3(function (t,
   dir,
   _v16) {
      return function () {
         return function () {
            var rotVel = A3(actorRotationalVelocity,
            t,
            dir,
            _v16);
            return _v16.rot.angle + rotVel;
         }();
      }();
   });
   var rotateActor = F3(function (t,
   dir,
   _v18) {
      return function () {
         return function () {
            var rot$ = _U.replace([["angle"
                                   ,A3(actorAngle,t,dir,_v18)]],
            _v18.rot);
            return _U.replace([["rot"
                               ,rot$]],
            _v18);
         }();
      }();
   });
   var stepBullets = F2(function (t,
   bullets) {
      return $List.filter(onCanvas)($List.map(function (bullet) {
         return A2(rotateActor,
         t,
         {_: {},x: 0,y: 0})(A2(moveActor,
         t,
         {_: {},x: 0,y: 0})(bullet));
      })(bullets));
   });
   var stepPlayerBullets = F4(function (t,
   input,
   _v20,
   bullets) {
      return function () {
         return F2(function (x,y) {
            return _L.append(x,y);
         })(_v20.gun.firing && _U.eq(_v20.gun.timeSince,
         0) ? $GameModel.createPlayerBullets(_v20) : _L.fromArray([]))(stepBullets(t)(bullets));
      }();
   });
   var stepEnemyBullets = F3(function (t,
   enemies,
   bullets) {
      return F2(function (x,y) {
         return _L.append(x,y);
      })($List.concatMap(function (enemy) {
         return fireEnemyBullets(t)(enemy);
      })(enemies))(stepBullets(t)(bullets));
   });
   var stepPlayer = F5(function (t,
   _v22,
   enemyBullets,
   score,
   player) {
      return function () {
         return function () {
            var rot$ = {_: {}
                       ,x: $Basics.toFloat(_v22.rot)
                       ,y: 0};
            var dir$ = _U.replace([["x"
                                   ,$Basics.toFloat(_v22.dir.x)]
                                  ,["y"
                                   ,$Basics.toFloat(_v22.dir.y)]],
            _v22.dir);
            return stepPlayerSpriteLivesSides(A2(stepActorLivesWithCooldown,
            t,
            enemyBullets)(stepActorLivesCooldown(t)(A2(stepPlayerGun,
            t,
            _v22.fire1)(A2(rotateActor,
            t,
            rot$)(A2(moveActor,
            t,
            dir$)(player))))));
         }();
      }();
   });
   var stepEnemy = F4(function (t,
   player,
   playerBullets,
   enemy) {
      return function () {
         var dir = A2(unitVectorTwoPoints,
         player.pos,
         enemy.pos);
         return killEnemy(playerBullets)(stepGun(t)(A2(rotateActor,
         t,
         enemy.dir)(A2(moveActor,
         t,
         enemy.dir)(avoidEdgesActor(t)(enemy)))));
      }();
   });
   var stepEnemies = F6(function (t,
   seeds,
   player,
   playerBullets,
   spawnCooldown,
   enemies) {
      return $List.map(function (enemy) {
         return A3(stepEnemy,
         t,
         player,
         playerBullets)(enemy);
      })(A3(spawnEnemyBatch,
      seeds,
      spawnCooldown,
      player)($List.filter(function ($) {
         return $Basics.not(isActorKilled($));
      })(enemies)));
   });
   var stepGame = F2(function (_v24,
   _v25) {
      return function () {
         return function () {
            return _U.eq(_v25.state,
            $GameModel.Pause) && _v24.userInput.unpause ? _U.replace([["state"
                                                                      ,$GameModel.Play]],
            _v25) : _U.eq(_v25.state,
            $GameModel.Pause) ? _v25 : _U.eq(_v25.state,
            $GameModel.Lost) ? _v25 : _U.cmp(_v25.player.lives,
            0) < 1 ? _U.replace([["state"
                                 ,$GameModel.Lost]],
            _v25) : function () {
               var seeds$ = {_: {}
                            ,a: A2($Pseudorandom.get,
                            _v25.seeds.a,
                            $Pseudorandom.$int)
                            ,b: A2($Pseudorandom.get,
                            _v25.seeds.b,
                            $Pseudorandom.$int)
                            ,c: A2($Pseudorandom.get,
                            _v25.seeds.c,
                            $Pseudorandom.$int)
                            ,d: A2($Pseudorandom.get,
                            _v25.seeds.d,
                            $Pseudorandom.$int)};
               var ui$ = A2(stepUI,
               _v24.timeDelta,
               _v25.player)(_v25.ui);
               var spawnCooldown$ = A2(stepSpawnCooldown,
               _v24.timeDelta,
               _v25.score)(_v25.spawnCooldown);
               var score$ = A2(stepScore,
               _v24.timeDelta,
               _v25.enemies)(_v25.score);
               var enemyBullets$ = A2(stepEnemyBullets,
               _v24.timeDelta,
               _v25.enemies)(_v25.enemyBullets);
               var enemies$ = A5(stepEnemies,
               _v24.timeDelta,
               _v25.seeds,
               _v25.player,
               _v25.playerBullets,
               _v25.spawnCooldown)(_v25.enemies);
               var playerBullets$ = A3(stepPlayerBullets,
               _v24.timeDelta,
               _v24.userInput,
               _v25.player)(_v25.playerBullets);
               var player$ = A4(stepPlayer,
               _v24.timeDelta,
               _v24.userInput,
               _v25.enemyBullets,
               _v25.score)(_v25.player);
               return _U.replace([["player"
                                  ,player$]
                                 ,["playerBullets"
                                  ,playerBullets$]
                                 ,["enemies",enemies$]
                                 ,["enemyBullets",enemyBullets$]
                                 ,["score",score$]
                                 ,["spawnCooldown"
                                  ,spawnCooldown$]
                                 ,["ui",ui$]
                                 ,["seeds",seeds$]],
               _v25);
            }();
         }();
      }();
   });
   _elm.GameUpdates.values = {_op: _op
                             ,tendTo: tendTo
                             ,tendToClose: tendToClose
                             ,near: near
                             ,outside: outside
                             ,isTrue: isTrue
                             ,isFalse: isFalse
                             ,onCanvas: onCanvas
                             ,dist: dist
                             ,unitVector: unitVector
                             ,unitVectorTwoPoints: unitVectorTwoPoints
                             ,collides: collides
                             ,collider: collider
                             ,movePos: movePos
                             ,accelerate: accelerate
                             ,actorPosition: actorPosition
                             ,actorVelocity: actorVelocity
                             ,actorRotationalVelocity: actorRotationalVelocity
                             ,actorAngle: actorAngle
                             ,moveActor: moveActor
                             ,rotateActor: rotateActor
                             ,stepActorLives: stepActorLives
                             ,stepActorLivesCooldown: stepActorLivesCooldown
                             ,stepActorLivesWithCooldown: stepActorLivesWithCooldown
                             ,fireGun: fireGun
                             ,reloadGun: reloadGun
                             ,stepGun: stepGun
                             ,stepBullets: stepBullets
                             ,stepPlayerGun: stepPlayerGun
                             ,stepPlayerSpriteLivesSides: stepPlayerSpriteLivesSides
                             ,stepPlayer: stepPlayer
                             ,stepPlayerBullets: stepPlayerBullets
                             ,killEnemy: killEnemy
                             ,avoidEdgesActor: avoidEdgesActor
                             ,stepEnemy: stepEnemy
                             ,isActorKilled: isActorKilled
                             ,spawnEnemyBatch: spawnEnemyBatch
                             ,stepEnemies: stepEnemies
                             ,fireEnemyBullets: fireEnemyBullets
                             ,stepEnemyBullets: stepEnemyBullets
                             ,addScoreForKill: addScoreForKill
                             ,stepScore: stepScore
                             ,stepSpawnCooldown: stepSpawnCooldown
                             ,stepUI: stepUI
                             ,stepGame: stepGame};
   return _elm.GameUpdates.values;
};Elm.GameModel = Elm.GameModel || {};
Elm.GameModel.make = function (_elm) {
   "use strict";
   _elm.GameModel = _elm.GameModel || {};
   if (_elm.GameModel.values)
   return _elm.GameModel.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameModel",
   $Basics = Elm.Basics.make(_elm),
   $GameDraw = Elm.GameDraw.make(_elm),
   $GameEnemies = Elm.GameEnemies.make(_elm),
   $List = Elm.List.make(_elm);
   var setActorBulletVelocity = F4(function (angle$,
   speed$,
   sides,
   _v0) {
      return function () {
         return function () {
            var vel$ = _U.replace([["vx"
                                   ,speed$ * $Basics.cos(angle$)]
                                  ,["vy"
                                   ,speed$ * $Basics.sin(angle$)]],
            _v0.vel);
            return _U.replace([["vel",vel$]
                              ,["speed",speed$]],
            _v0);
         }();
      }();
   });
   var setActorBulletAngle = F2(function (angle$,
   _v2) {
      return function () {
         return function () {
            var rot$ = _U.replace([["angle"
                                   ,angle$]],
            _v2.rot);
            return _U.replace([["rot"
                               ,rot$]],
            _v2);
         }();
      }();
   });
   var createActor = F3(function (x,
   y,
   spr) {
      return {_: {}
             ,accel: 0
             ,deccel: 0
             ,dir: {_: {},x: 0,y: 0}
             ,gun: {_: {}
                   ,fireRate: 0
                   ,firing: false
                   ,timeSince: 0}
             ,kill: false
             ,lives: 1
             ,loseLifeCooldown: 0
             ,loseLifeCooldownMax: 0.5
             ,pos: {_: {},x: x,y: y}
             ,rot: {_: {}
                   ,accel: 0
                   ,angle: 0
                   ,deccel: 0
                   ,speed: 0
                   ,vel: 0}
             ,speed: 0
             ,spr: spr
             ,vel: {_: {},vx: 0,vy: 0}};
   });
   var createPlayer = F2(function (x,
   y) {
      return function () {
         var actor = A3(createActor,
         x,
         y,
         $GameDraw.player);
         return _U.replace([["rot"
                            ,{_: {}
                             ,accel: $Basics.pi / 14
                             ,angle: 0
                             ,deccel: $Basics.pi / 20
                             ,speed: $Basics.pi / 4
                             ,vel: 0}]
                           ,["speed",200]
                           ,["accel",50]
                           ,["deccel",50]
                           ,["gun"
                            ,{_: {}
                             ,fireRate: 12
                             ,firing: false
                             ,timeSince: 0}]
                           ,["lives",4]],
         actor);
      }();
   });
   var createEnemy = F3(function (x,
   y,
   formationDir$) {
      return function () {
         var actor = A3(createActor,
         x,
         y,
         $GameDraw.enemy1);
         return _U.replace([["rot"
                            ,{_: {}
                             ,accel: $Basics.pi / 14
                             ,angle: 0
                             ,deccel: $Basics.pi / 20
                             ,speed: $Basics.pi / 4
                             ,vel: $Basics.pi / 10}]
                           ,["speed",80]
                           ,["accel",20]
                           ,["deccel",5]
                           ,["gun"
                            ,{_: {}
                             ,fireRate: 0.534
                             ,firing: true
                             ,timeSince: 1}]
                           ,["dir",formationDir$]],
         actor);
      }();
   });
   var createEnemyBatch = F3(function (formation,
   pos,
   dir) {
      return $List.map(function (pos$) {
         return A3(createEnemy,
         pos.x + pos$.x,
         pos.y + pos$.y,
         dir);
      })(formation);
   });
   var createActorBullet = F4(function (_v4,
   spr,
   speed$,
   side) {
      return function () {
         return function () {
            var bullet = A3(createActor,
            _v4.pos.x,
            _v4.pos.y,
            spr);
            var angle$ = _v4.rot.angle + 2 * $Basics.pi * $Basics.toFloat(side) / $Basics.toFloat(_v4.spr.sides);
            return A3(setActorBulletVelocity,
            angle$,
            speed$,
            _v4.spr.sides)(setActorBulletAngle(angle$)(bullet));
         }();
      }();
   });
   var createActorBullets = F3(function (actor,
   spr,
   speed) {
      return function () {
         var num = actor.spr.sides;
         return $List.map(function (n) {
            return A4(createActorBullet,
            actor,
            spr,
            speed,
            n);
         })(_L.range(1,num));
      }();
   });
   var createPlayerBullets = function (player) {
      return A3(createActorBullets,
      player,
      $GameDraw.playerBullet1,
      600);
   };
   var createEnemyBullets = function (enemy) {
      return A3(createActorBullets,
      enemy,
      $GameDraw.enemyBullet1(enemy),
      100);
   };
   var $ = {ctor: "_Tuple2"
           ,_0: 400
           ,_1: 300},
   halfWidth = $._0,
   halfHeight = $._1;
   var createPlayerLivesUI = function (num) {
      return function () {
         var step = 60;
         var $ = {ctor: "_Tuple2"
                 ,_0: 0 - halfWidth + 10
                 ,_1: halfHeight - 10},
         x = $._0,
         y = $._1;
         return $List.map(function (n) {
            return A3(createActor,
            $Basics.toFloat(x) + $Basics.toFloat(n - 1) * step,
            $Basics.toFloat(y),
            $GameDraw.playerLife);
         })(_L.range(1,num));
      }();
   };
   var $ = {ctor: "_Tuple2"
           ,_0: 800
           ,_1: 600},
   gameWidth = $._0,
   gameHeight = $._1;
   var GameState = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,enemies: d
             ,enemyBullets: e
             ,player: b
             ,playerBullets: c
             ,score: f
             ,seeds: i
             ,spawnCooldown: g
             ,state: a
             ,ui: h};
   });
   var Lost = {ctor: "Lost"};
   var Won = {ctor: "Won"};
   var Pause = {ctor: "Pause"};
   var defaultGame = function () {
      var player$ = A2(createPlayer,
      0,
      0);
      return {_: {}
             ,enemies: _L.fromArray([])
             ,enemyBullets: _L.fromArray([])
             ,player: player$
             ,playerBullets: _L.fromArray([])
             ,score: 0
             ,seeds: {_: {}
                     ,a: 1
                     ,b: 2
                     ,c: 3
                     ,d: 4}
             ,spawnCooldown: 0
             ,state: Pause
             ,ui: {_: {}
                  ,lives: createPlayerLivesUI(player$.lives)}};
   }();
   var Play = {ctor: "Play"};
   var Seeds = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,a: a
             ,b: b
             ,c: c
             ,d: d};
   });
   var Level = F2(function (a,b) {
      return {_: {}
             ,enemies: b
             ,name: a};
   });
   var UI = function (a) {
      return {_: {},lives: a};
   };
   var EnemyGroup = F3(function (a,
   b,
   c) {
      return {_: {}
             ,enemies: a
             ,formation: b
             ,target: c};
   });
   var Actor = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return function (m) {
                                          return {_: {}
                                                 ,accel: e
                                                 ,deccel: f
                                                 ,dir: m
                                                 ,gun: h
                                                 ,kill: l
                                                 ,lives: i
                                                 ,loseLifeCooldown: j
                                                 ,loseLifeCooldownMax: k
                                                 ,pos: a
                                                 ,rot: c
                                                 ,speed: d
                                                 ,spr: g
                                                 ,vel: b};
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var Gun = F3(function (a,b,c) {
      return {_: {}
             ,fireRate: b
             ,firing: a
             ,timeSince: c};
   });
   var Rotation = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,accel: d
             ,angle: a
             ,deccel: e
             ,speed: b
             ,vel: c};
   });
   var Velocity = F2(function (a,
   b) {
      return {_: {},vx: a,vy: b};
   });
   var Direction = F2(function (a,
   b) {
      return {_: {},x: a,y: b};
   });
   var Position = F2(function (a,
   b) {
      return {_: {},x: a,y: b};
   });
   _elm.GameModel.values = {_op: _op
                           ,Position: Position
                           ,Direction: Direction
                           ,Velocity: Velocity
                           ,Rotation: Rotation
                           ,Gun: Gun
                           ,Actor: Actor
                           ,EnemyGroup: EnemyGroup
                           ,UI: UI
                           ,Level: Level
                           ,Seeds: Seeds
                           ,Play: Play
                           ,Pause: Pause
                           ,Won: Won
                           ,Lost: Lost
                           ,GameState: GameState
                           ,gameHeight: gameHeight
                           ,gameWidth: gameWidth
                           ,halfHeight: halfHeight
                           ,halfWidth: halfWidth
                           ,createActor: createActor
                           ,createPlayer: createPlayer
                           ,createEnemy: createEnemy
                           ,createEnemyBatch: createEnemyBatch
                           ,setActorBulletAngle: setActorBulletAngle
                           ,setActorBulletVelocity: setActorBulletVelocity
                           ,createActorBullet: createActorBullet
                           ,createActorBullets: createActorBullets
                           ,createPlayerBullets: createPlayerBullets
                           ,createEnemyBullets: createEnemyBullets
                           ,createPlayerLivesUI: createPlayerLivesUI
                           ,defaultGame: defaultGame};
   return _elm.GameModel.values;
};Elm.GameDraw = Elm.GameDraw || {};
Elm.GameDraw.make = function (_elm) {
   "use strict";
   _elm.GameDraw = _elm.GameDraw || {};
   if (_elm.GameDraw.values)
   return _elm.GameDraw.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameDraw",
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm);
   var polySideLength = F2(function (sides,
   radius) {
      return 2 * radius * $Basics.sin($Basics.pi / sides);
   });
   var enemyColour2 = A3($Color.rgb,
   255,
   0,
   170);
   var enemyBullet1 = function (enemy) {
      return function () {
         var sideLen = A2(polySideLength,
         $Basics.toFloat(enemy.spr.sides),
         $Basics.toFloat(enemy.spr.size));
         return {_: {}
                ,shape: $Graphics$Collage.filled(enemyColour2)($Graphics$Collage.circle($Basics.toFloat(enemy.spr.sides) + 4))
                ,sides: 0
                ,size: 0};
      }();
   };
   var enemyColour1 = A3($Color.rgb,
   255,
   170,
   0);
   var enemy1 = function () {
      var size$ = 10;
      var sides$ = 3;
      return {_: {}
             ,shape: $Graphics$Collage.filled(enemyColour1)(A2($Graphics$Collage.ngon,
             sides$,
             size$))
             ,sides: sides$
             ,size: size$};
   }();
   var playerColour2 = A3($Color.rgb,
   0,
   170,
   255);
   var playerLife = function () {
      var size$ = 15;
      var sides$ = 5;
      return {_: {}
             ,shape: $Graphics$Collage.filled(playerColour2)(A2($Graphics$Collage.ngon,
             sides$,
             size$))
             ,sides: sides$
             ,size: size$};
   }();
   var playerColour = A3($Color.rgb,
   170,
   255,
   0);
   var player = function () {
      var size$ = 15;
      var sides$ = 6;
      return {_: {}
             ,shape: $Graphics$Collage.filled(playerColour)(A2($Graphics$Collage.ngon,
             sides$,
             size$))
             ,sides: sides$
             ,size: size$};
   }();
   var playerSides = function (sides$) {
      return function () {
         var p = player;
         return _U.replace([["shape"
                            ,$Graphics$Collage.filled(playerColour)(A2($Graphics$Collage.ngon,
                            sides$,
                            $Basics.toFloat(p.size)))]
                           ,["sides",sides$]],
         p);
      }();
   };
   var playerBullet1 = function () {
      var sideLen = A2(polySideLength,
      $Basics.toFloat(player.sides),
      $Basics.toFloat(player.size));
      return {_: {}
             ,shape: A2($Graphics$Collage.filled,
             playerColour,
             A2($Graphics$Collage.rect,
             sideLen,
             4))
             ,sides: 0
             ,size: $Basics.floor(4)};
   }();
   var Sprite = F3(function (a,
   b,
   c) {
      return {_: {}
             ,shape: a
             ,sides: b
             ,size: c};
   });
   _elm.GameDraw.values = {_op: _op
                          ,Sprite: Sprite
                          ,playerColour: playerColour
                          ,playerColour2: playerColour2
                          ,enemyColour1: enemyColour1
                          ,enemyColour2: enemyColour2
                          ,polySideLength: polySideLength
                          ,player: player
                          ,playerSides: playerSides
                          ,playerLife: playerLife
                          ,enemy1: enemy1
                          ,playerBullet1: playerBullet1
                          ,enemyBullet1: enemyBullet1};
   return _elm.GameDraw.values;
};Elm.GameEnemies = Elm.GameEnemies || {};
Elm.GameEnemies.make = function (_elm) {
   "use strict";
   _elm.GameEnemies = _elm.GameEnemies || {};
   if (_elm.GameEnemies.values)
   return _elm.GameEnemies.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameEnemies",
   $Basics = Elm.Basics.make(_elm);
   var createGroup = F2(function (formation,
   movementPath) {
      return _L.fromArray([]);
   });
   var linearPath = function (grad) {
      return _L.fromArray([]);
   };
   var spiralFormation = _L.fromArray([]);
   var circleFormation = _L.fromArray([]);
   var squareFormation = F2(function (len,
   spacing) {
      return _L.fromArray([{_: {}
                           ,x: 0 - spacing
                           ,y: 0 - spacing}
                          ,{_: {},x: 0 - spacing,y: 0}
                          ,{_: {}
                           ,x: 0 - spacing
                           ,y: spacing}
                          ,{_: {},x: 0,y: spacing}
                          ,{_: {},x: spacing,y: spacing}
                          ,{_: {},x: spacing,y: 0}
                          ,{_: {}
                           ,x: spacing
                           ,y: 0 - spacing}
                          ,{_: {},x: 0,y: 0 - spacing}]);
   });
   var generate = F2(function (difficulty,
   spacing) {
      return A2(squareFormation,
      difficulty + 2,
      spacing);
   });
   var modVal = F2(function (divisor,
   val) {
      return _U.eq(A2($Basics._op["%"],
      val,
      divisor),
      0) || _U.eq(val,
      0) ? true : false;
   });
   _elm.GameEnemies.values = {_op: _op
                             ,modVal: modVal
                             ,squareFormation: squareFormation
                             ,circleFormation: circleFormation
                             ,spiralFormation: spiralFormation
                             ,linearPath: linearPath
                             ,createGroup: createGroup
                             ,generate: generate};
   return _elm.GameEnemies.values;
};Elm.GameInput = Elm.GameInput || {};
Elm.GameInput.make = function (_elm) {
   "use strict";
   _elm.GameInput = _elm.GameInput || {};
   if (_elm.GameInput.values)
   return _elm.GameInput.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "GameInput",
   $Keyboard = Elm.Keyboard.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Time = Elm.Time.make(_elm);
   var Input = F2(function (a,b) {
      return {_: {}
             ,timeDelta: a
             ,userInput: b};
   });
   var fireDelta = $Time.fps(3);
   var UserInput = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,dir: a
             ,fire1: c
             ,rot: b
             ,unpause: d};
   });
   var userInput = function () {
      var unpause = $Keyboard.space;
      var fire1 = $Signal.constant(true);
      var rot = A2($Signal.lift,
      function (_) {
         return _.x;
      },
      $Keyboard.wasd);
      var dir = $Keyboard.arrows;
      return A2($Signal._op["~"],
      A2($Signal._op["~"],
      A2($Signal._op["~"],
      A2($Signal._op["<~"],
      UserInput,
      dir),
      rot),
      fire1),
      unpause);
   }();
   var Direction = F2(function (a,
   b) {
      return {_: {},x: a,y: b};
   });
   _elm.GameInput.values = {_op: _op
                           ,Direction: Direction
                           ,UserInput: UserInput
                           ,fireDelta: fireDelta
                           ,userInput: userInput
                           ,Input: Input};
   return _elm.GameInput.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   if (_elm.Pseudorandom.values)
   return _elm.Pseudorandom.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom",
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Pseudorandom$Infix = Elm.Pseudorandom.Infix.make(_elm),
   $Pseudorandom$Internal = Elm.Pseudorandom.Internal.make(_elm);
   var get = F2(function (n,r) {
      return $Basics.fst(r(n));
   });
   var $int = function (r) {
      return function () {
         var s$ = $Pseudorandom$Internal.xorshift(r);
         return {ctor: "_Tuple2"
                ,_0: s$
                ,_1: s$};
      }();
   };
   var $float = function ($) {
      return $Pseudorandom$Internal.first(function (n$) {
         return $Basics.toFloat($Basics.abs(n$) - 1) / (0 - $Pseudorandom$Internal.minInt);
      })($int($));
   };
   var range = function (rn) {
      return function ($) {
         return $Pseudorandom$Internal.first($Pseudorandom$Internal.roundClamp(rn))($int($));
      };
   };
   var andThen = F2(function (x,
   y) {
      return A2($Pseudorandom$Infix._op["=<<"],
      x,
      y);
   });
   var apply = F2(function (x,y) {
      return A2($Pseudorandom$Infix._op["<*>"],
      x,
      y);
   });
   var constant = F2(function (v0,
   v1) {
      return {ctor: "_Tuple2"
             ,_0: v0
             ,_1: v1};
   });
   var combine = A2($List.foldr,
   F2(function (x,xs) {
      return A2($Pseudorandom$Infix._op["<*>"],
      A2($Pseudorandom$Infix._op["<$>"],
      F2(function (x,y) {
         return A2($List._op["::"],
         x,
         y);
      }),
      x),
      xs);
   }),
   constant(_L.fromArray([])));
   var map = function (f) {
      return function ($) {
         return combine($List.map(f)($));
      };
   };
   var lift = F2(function (x,y) {
      return A2($Pseudorandom$Infix._op["<$>"],
      x,
      y);
   });
   _elm.Pseudorandom.values = {_op: _op
                              ,constant: constant
                              ,andThen: andThen
                              ,combine: combine
                              ,map: map
                              ,$int: $int
                              ,$float: $float
                              ,range: range
                              ,get: get
                              ,lift: lift
                              ,apply: apply};
   return _elm.Pseudorandom.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.Infix = Elm.Pseudorandom.Infix || {};
Elm.Pseudorandom.Infix.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   _elm.Pseudorandom.Infix = _elm.Pseudorandom.Infix || {};
   if (_elm.Pseudorandom.Infix.values)
   return _elm.Pseudorandom.Infix.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom.Infix",
   $Basics = Elm.Basics.make(_elm),
   $Pseudorandom$Internal = Elm.Pseudorandom.Internal.make(_elm);
   _op["=<<"] = F2(function (f,m) {
      return function ($) {
         return $Basics.uncurry(f)(m($));
      };
   });
   _op["<=<"] = F3(function (f,
   g,
   x) {
      return A2(_op["=<<"],f,g(x));
   });
   _op["<*>"] = F2(function (rf,
   ra) {
      return function ($) {
         return function (_v0) {
            return function () {
               switch (_v0.ctor)
               {case "_Tuple2":
                  return $Pseudorandom$Internal.first(F2(function (x,
                    y) {
                       return y(x);
                    })(_v0._0))(rf(_v0._1));}
               _E.Case($moduleName,
               "on line 23, column 25 to 47");
            }();
         }(ra($));
      };
   });
   _op["<$>"] = F2(function (f,r) {
      return function ($) {
         return $Pseudorandom$Internal.first(f)(r($));
      };
   });
   _elm.Pseudorandom.Infix.values = {_op: _op};
   return _elm.Pseudorandom.Infix.values;
};Elm.Pseudorandom = Elm.Pseudorandom || {};
Elm.Pseudorandom.Internal = Elm.Pseudorandom.Internal || {};
Elm.Pseudorandom.Internal.make = function (_elm) {
   "use strict";
   _elm.Pseudorandom = _elm.Pseudorandom || {};
   _elm.Pseudorandom.Internal = _elm.Pseudorandom.Internal || {};
   if (_elm.Pseudorandom.Internal.values)
   return _elm.Pseudorandom.Internal.values;
   var _op = {},
   _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _A = _N.Array.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Pseudorandom.Internal",
   $Basics = Elm.Basics.make(_elm),
   $Bitwise = Elm.Bitwise.make(_elm);
   var roundClamp = F2(function (_v0,
   i) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return _v0._0 + A2($Basics._op["%"],
              i - _v0._0,
              _v0._1 - _v0._0 + 1);}
         _E.Case($moduleName,
         "on line 26, column 23 to 47");
      }();
   });
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var bit32 = 4294967295;
   var c = 5;
   var b = 17;
   var a = 13;
   var xorshift = function (s) {
      return function () {
         var x = A2($Bitwise.xor,
         s,
         A2($Bitwise.shiftLeft,s,a));
         var y = A2($Bitwise.xor,
         x,
         A2($Bitwise.shiftRight,x,b));
         return A2($Bitwise.xor,
         y,
         A2($Bitwise.shiftLeft,y,c));
      }();
   };
   var first = F2(function (f,
   _v4) {
      return function () {
         switch (_v4.ctor)
         {case "_Tuple2":
            return {ctor: "_Tuple2"
                   ,_0: f(_v4._0)
                   ,_1: _v4._1};}
         _E.Case($moduleName,
         "on line 6, column 19 to 25");
      }();
   });
   _elm.Pseudorandom.Internal.values = {_op: _op
                                       ,first: first
                                       ,xorshift: xorshift
                                       ,roundClamp: roundClamp
                                       ,maxInt: maxInt
                                       ,minInt: minInt};
   return _elm.Pseudorandom.Internal.values;
};