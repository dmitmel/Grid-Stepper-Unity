//
//  Copyright (c) 2016 Dmytro Meleshko
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

using UnityEngine;
using System.Collections.Generic;
using System.Linq;
using System;

namespace GridStepper {
    [Serializable]
    public struct CubeCoord {
        public int x;
        public int y;

        public CubeCoord(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public CubeCoord Move(int x, int y) {
            return new CubeCoord(this.x + x, this.y + y);
        }

        public override string ToString() {
            return string.Format("(x: {0}, y: {1})", x, y);
        }
    }

    public enum CubeType {
        Start, NoCube, Cube, Finish
    }

    public class Cube {
        public readonly CubeType type;
        public bool isVisited;

        public Cube(CubeType type) {
            this.type = type;
            isVisited = false;
        }

        public static Cube startCube {
            get {
                var result = new Cube(CubeType.Start);
                result.isVisited = true;
                return result;
            }
        }

        public static Cube finishCube {
            get {
                return new Cube(CubeType.Finish);
            }
        }

        public static Cube noCube {
            get {
                return new Cube(CubeType.NoCube);
            }
        }

        public static Cube cube {
            get {
                return new Cube(CubeType.Cube);
            }
        }

        public override string ToString() {
            return string.Format("Cube[type: {0}, isVisited: {1}]", type, isVisited);
        }
    }

    public class GameController : Singleton<GameController> {
        public int levelWidth;
        public int levelHeight;

        public GameObject cubesObject;
        public Dictionary<CubeCoord, Cube> cubes;

        void Start() {
            cubes = new Dictionary<CubeCoord, Cube>(levelWidth * levelHeight);

            foreach (Transform cube in cubesObject.transform) {
                var x = (int) cube.localPosition.x;
                var y = (int) cube.localPosition.z;     // Так как Vector3.y отвечает за высоту, я использую Vector3.z для CubeCoord.y
                var cubeCoord = new CubeCoord(x, y);

                switch (cube.tag) {
                    case "Cube":
                        cubes[cubeCoord] = Cube.cube;
                        break;
                    case "StartCube":
                        cubes[cubeCoord] = Cube.startCube;
                        break;
                    case "FinishCube":
                        cubes[cubeCoord] = Cube.finishCube;
                        break;
                    default:
                        Debug.LogError("[GameController] Неизвестный тег куба '" + cube.tag + "'!");
                        cubes[cubeCoord] = Cube.noCube;
                        break;
                }
            }

            foreach (int x in Enumerable.Range(0, levelWidth)) {
                foreach (int y in Enumerable.Range(0, levelHeight)) {
                    CubeCoord coord = new CubeCoord(x, y);
                    if (!cubes.ContainsKey(coord))
                        cubes[coord] = Cube.noCube;
                }
            }
        }
    }
}
