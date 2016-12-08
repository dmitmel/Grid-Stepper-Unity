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

using System;

namespace GridStepper {
    /**
     * Enum для направлений чего угодно: поворота, направления движения, etc.
     */
    public enum Direction {
        Up, Left, Down, Right
    }

    public static class Directions {
        /**
         * Возвращает направление справа относительно уже существуюшего.
         */
        public static Direction GetRight(this Direction direction) {
            switch (direction) {
                case Direction.Up:
                    return Direction.Right;
                case Direction.Right:
                    return Direction.Down;
                case Direction.Down:
                    return Direction.Left;
                case Direction.Left:
                    return Direction.Up;
                default:
                    throw new Exception("Unreachable code");
            }
        }

        /**
         * Возвращает направление слева относительно уже существуюшего.
         */
        public static Direction GetLeft(this Direction direction) {
            switch (direction) {
                case Direction.Up:
                    return Direction.Left;
                case Direction.Left:
                    return Direction.Down;
                case Direction.Down:
                    return Direction.Right;
                case Direction.Right:
                    return Direction.Up;
                default:
                    throw new Exception("Unreachable code");
            }
        }

        /**
         * Возвращает противоположное направление относительно уже существуюшего.
         */
        public static Direction GetOpposite(this Direction direction) {
            switch (direction) {
                case Direction.Up:
                    return Direction.Down;
                case Direction.Right:
                    return Direction.Left;
                case Direction.Down:
                    return Direction.Up;
                case Direction.Left:
                    return Direction.Right;
                default:
                    throw new Exception("Unreachable code");
            }
        }

        /**
         * Проверяет, находится ли other справа от current.
         */
        public static bool IsRightFor(this Direction other, Direction current) {
            return other == current.GetRight();
        }

        /**
         * Проверяет, находится ли other слева от current.
         */
        public static bool IsLeftFor(this Direction other, Direction current) {
            return other == current.GetLeft();
        }

        /**
         * Проверяет, является ли other противоположным для current.
         */
        public static bool IsOppositeFor(this Direction other, Direction current) {
            return other == current.GetOpposite();
        }
    }
}
