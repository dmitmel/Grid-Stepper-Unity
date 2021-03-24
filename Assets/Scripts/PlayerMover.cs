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
using System.Collections;
using System;
using System.Collections.Generic;
using System.Linq;

namespace GridStepper {
    public class PlayerMover : Singleton<PlayerMover> {
        public Transform moverTransform;
        public GameObject pathCube;
        public Transform pathCubes;

        public int moveSteps;
        public float rotationSteps;
        public float secondsBetweenActions;

        public CubeCoord cubeCoord;

        bool playerIsMoving;
        Direction direction = Direction.Up;

        bool isOnFinishCube;

        void FixedUpdate() {
            if (!playerIsMoving && (Input.GetKey(KeyCode.W) ||
                                    Input.GetKey(KeyCode.A) ||
                                    Input.GetKey(KeyCode.S) ||
                                    Input.GetKey(KeyCode.D))) {
                Direction nextDirection = GetNextDirection();
                CubeCoord nextCubeCoord = GetNextCubeCoord();

                if (nextCubeCoord.x >= 0 && nextCubeCoord.x < GameController.Instance.levelWidth
                 && nextCubeCoord.y >= 0 && nextCubeCoord.y < GameController.Instance.levelHeight) {
                    Cube nextCube = GameController.Instance.cubes[nextCubeCoord];

                    if (nextCube.type != CubeType.NoCube && !nextCube.isVisited) {
                        playerIsMoving = true;
                        nextCube.isVisited = true;

                        if (nextCube.type == CubeType.Finish)
                            isOnFinishCube = true;

                        switch (nextDirection) {
                            case Direction.Left:
                                StartCoroutine(MoveLeft());
                                break;
                            case Direction.Right:
                                StartCoroutine(MoveRight());
                                break;
                            case Direction.Down:
                                StartCoroutine(MoveDown());
                                break;
                            case Direction.Up:
                                StartCoroutine(MoveUp());
                                break;
                        }
                    }
                }
            }
        }

        Direction GetNextDirection() {
            if (Input.GetKey(KeyCode.A))
                return Direction.Left;
            else if (Input.GetKey(KeyCode.D))
                return Direction.Right;
            else if (Input.GetKey(KeyCode.S))
                return Direction.Down;
            else if (Input.GetKey(KeyCode.W))
                return Direction.Up;
            else
                throw new Exception("Unreachable code");
        }

        CubeCoord GetNextCubeCoord() {
            if (Input.GetKey(KeyCode.A))
                return cubeCoord.Move(-1, 0);
            else if (Input.GetKey(KeyCode.D))
                return cubeCoord.Move(1, 0);
            else if (Input.GetKey(KeyCode.S))
                return cubeCoord.Move(0, -1);
            else if (Input.GetKey(KeyCode.W))
                return cubeCoord.Move(0, 1);
            else
                throw new Exception("Unreachable code");
        }

        IEnumerator MoveUp() {
            yield return StartCoroutine(TurnTo(Direction.Up));
            yield return StartCoroutine(MoveForwards(Vector3.forward));
            yield return StartCoroutine(FinishMovement());
            cubeCoord = cubeCoord.Move(0, 1);
        }

        IEnumerator MoveRight() {
            yield return StartCoroutine(TurnTo(Direction.Right));
            yield return StartCoroutine(MoveForwards(Vector3.right));
            yield return StartCoroutine(FinishMovement());
            cubeCoord = cubeCoord.Move(1, 0);
        }

        IEnumerator MoveDown() {
            yield return StartCoroutine(TurnTo(Direction.Down));
            yield return StartCoroutine(MoveForwards(Vector3.back));
            yield return StartCoroutine(FinishMovement());
            cubeCoord = cubeCoord.Move(0, -1);
        }

        IEnumerator MoveLeft() {
            yield return StartCoroutine(TurnTo(Direction.Left));
            yield return StartCoroutine(MoveForwards(Vector3.left));
            yield return StartCoroutine(FinishMovement());
            cubeCoord = cubeCoord.Move(-1, 0);
        }

        IEnumerator MoveForwards(Vector3 increment) {
            Instantiate(pathCube, moverTransform.position, Quaternion.identity, pathCubes);
            yield return StartCoroutine(moverTransform.TranslateAnimation(moveSteps / 2, moverTransform.position + increment / 2));
            Instantiate(pathCube, moverTransform.position, Quaternion.identity, pathCubes);
            yield return StartCoroutine(moverTransform.TranslateAnimation(moveSteps / 2, moverTransform.position + increment / 2));
        }

        IEnumerator FinishMovement() {
            if (isOnFinishCube) {
                if (IsGameCompleted()) {
                    Instantiate(pathCube, moverTransform.position, Quaternion.identity, pathCubes);
                    this.enabled = false;
                    GetComponent<PlayerFinishAnimation>().enabled = true;
                }
            } else {
                yield return new WaitForSeconds(secondsBetweenActions);
                playerIsMoving = false;
            }
        }

        bool IsGameCompleted() {
            foreach (KeyValuePair<CubeCoord, Cube> cubeCoordAndCube in GameController.Instance.cubes) {
                var cube = cubeCoordAndCube.Value;
                if (cube.type == CubeType.Cube && !cube.isVisited)
                    return false;
            }

            return true;
        }

        IEnumerator TurnTo(Direction direction) {
            if (direction.IsLeftFor(this.direction)) {
                yield return StartCoroutine(TurnLeft());
            } else if (direction.IsRightFor(this.direction)) {
                yield return StartCoroutine(TurnRight());
            } else if (direction.IsOppositeFor(this.direction)) {
                bool turnRight = RandomBoolean();
                if (turnRight) {
                    yield return StartCoroutine(TurnRight());
                    yield return StartCoroutine(TurnRight());
                } else {
                    yield return StartCoroutine(TurnLeft());
                    yield return StartCoroutine(TurnLeft());
                }
            }
        }

        bool RandomBoolean() {
            return (UnityEngine.Random.Range(0, 2) == 1);
        }

        IEnumerator TurnLeft() {
            for (float i = 0; i < rotationSteps; i++) {
                moverTransform.Rotate(0, -90 / rotationSteps, 0);
                yield return null;
            }

            direction = Directions.GetLeft(direction);
        }

        IEnumerator TurnRight() {
            for (float i = 0; i < rotationSteps; i++) {
                moverTransform.Rotate(0, 90 / rotationSteps, 0);
                yield return null;
            }

            direction = Directions.GetRight(direction);
        }
    }
}
