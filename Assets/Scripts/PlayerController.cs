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

namespace GridStepper {
    public class PlayerController : Singleton<PlayerController> {
        private static float ROTATION_ANGLE = 90;

        public float moveStep;
        public float rotationStep;
        public float secondsBetweenActions;

        public int cubeX;
        public int cubeY;

        private bool freezeMovement = false;
        private Direction direction = Direction.Up;

        private bool isOnFinishCube = false;

        void Start() {
            if (ROTATION_ANGLE % rotationStep != 0.0) {
                Debug.LogError("[PlayerController] rotationStep (" + rotationStep + ") не является кратным 90! " +
                                                 " Из-за этого, положение при повороте игрока может отличатся от 90.");
            }

            if (moveStep == 0) {
                Debug.LogError("[PlayerController] moveStep (" + moveStep + ") равен 0! " +
                                                 " Из-за этого, игрок вообще не будет здвигатся за шаг.");
            }

            if (moveStep < 0) {
                Debug.LogError("[PlayerController] moveStep (" + moveStep + ") меньше 0! " +
                                                 " Из-за этого, игрок будет здвигатся за шаг назад.");
            }

            if (moveStep > 1) {
                Debug.LogError("[PlayerController] moveStep (" + moveStep + ") больше 1! " +
                                                 " Из-за этого, игрок будет здвигатся за шаг больше чем на 1.");
            }
        }

        void FixedUpdate() {
            if (!freezeMovement && Input.anyKey) {
                Direction nextDirection = Direction.Up;
                int nextCubeX = cubeX;
                int nextCubeY = cubeY;

                if (Input.GetKey(KeyCode.A)) {
                    nextCubeX = cubeX - 1;
                    nextDirection = Direction.Left;
                } else if (Input.GetKey(KeyCode.D)) {
                    nextCubeX = cubeX + 1;
                    nextDirection = Direction.Right;
                } else if (Input.GetKey(KeyCode.S)) {
                    nextCubeY = cubeY - 1;
                    nextDirection = Direction.Down;
                } else if (Input.GetKey(KeyCode.W)) {
                    nextCubeY = cubeY + 1;
                    nextDirection = Direction.Up;
                }

                if (nextCubeX >= 0 && nextCubeX < GameController.Instance.levelWidth
                 && nextCubeY >= 0 && nextCubeY < GameController.Instance.levelHeight) {
                    CubeCoord nextCubeCoord = new CubeCoord(nextCubeX, nextCubeY);
                    Cube nextCube = GameController.Instance.cubes[nextCubeCoord];

                    if (nextCube.type != CubeType.NoCube && !nextCube.isVisited) {
                        freezeMovement = true;
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

        private IEnumerator MoveUp() {
            yield return StartCoroutine(TurnTo(Direction.Up));
            yield return StartCoroutine(transform.TranslateAnimation(10, transform.position + Vector3.forward));
            yield return StartCoroutine(FinishMovement());
            cubeY++;
        }

        private IEnumerator MoveRight() {
            yield return StartCoroutine(TurnTo(Direction.Right));
            yield return StartCoroutine(transform.TranslateAnimation(10, transform.position + Vector3.forward));
            yield return StartCoroutine(FinishMovement());
            cubeX++;
        }

        private IEnumerator MoveDown() {
            yield return StartCoroutine(TurnTo(Direction.Down));
            yield return StartCoroutine(transform.TranslateAnimation(10, transform.position + Vector3.forward));
            yield return StartCoroutine(FinishMovement());
            cubeY--;
        }

        private IEnumerator MoveLeft() {
            yield return StartCoroutine(TurnTo(Direction.Left));
            yield return StartCoroutine(transform.TranslateAnimation(10, transform.position + Vector3.forward));
            yield return StartCoroutine(FinishMovement());
            cubeX--;
        }

        private IEnumerator FinishMovement() {
            if (isOnFinishCube) {
                //            GetComponent<FinishAnimation>().enabled = true;
            } else {
                yield return new WaitForSeconds(secondsBetweenActions);
                freezeMovement = false;
            }
        }

        private IEnumerator TurnTo(Direction newDirection) {
            if (Directions.IsLeftFor(this.direction, newDirection)) {
                yield return StartCoroutine(TurnLeft());
            } else if (Directions.IsRightFor(this.direction, newDirection)) {
                yield return StartCoroutine(TurnRight());
            } else if (Directions.IsOppositeFor(this.direction, newDirection)) {
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

        private bool RandomBoolean() {
            return (Random.Range(0, 2) == 1);
        }

        private IEnumerator TurnLeft() {
            for (float x = 0; x < ROTATION_ANGLE; x += rotationStep) {
                transform.Rotate(0, -rotationStep, 0);
                yield return null;
            }

            direction = Directions.GetLeft(direction);
        }

        private IEnumerator TurnRight() {
            for (float x = 0; x < ROTATION_ANGLE; x += rotationStep) {
                transform.Rotate(0, rotationStep, 0);
                yield return null;
            }

            direction = Directions.GetRight(direction);
        }
    }
}
