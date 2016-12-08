﻿//
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
using UnityEngine;
using System.Collections;

namespace GridStepper {
    public static class Animations {
        public static IEnumerator TranslateAnimation(this Transform transform, float stepCount, Vector3 dest) {
            Vector3 difference = dest - transform.position;
            Vector3 increment = difference / stepCount;

            for (int step = 0; step < stepCount; step++) {
                transform.Translate(increment, Space.World);
                yield return null;
            }

            transform.position = dest;
        }

        public static IEnumerator ScaleAnimation(this Transform transform, float stepCount, Vector3 dest) {
            Vector3 difference = dest - transform.localScale;
            Vector3 increment = difference / stepCount;

            for (int step = 0; step < stepCount; step++) {
                transform.localScale += increment;
                yield return null;
            }

            transform.position = dest;
        }
    }
}