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

namespace GridStepper {
    public class HoverEffect : MonoBehaviour {
        public float verticalSpeed;
        public float amplitude;
        public bool allowRandomDeflection;

        float randomDeflection;

        void Start() {
            randomDeflection = allowRandomDeflection ? Random.value * Mathf.PI * 2 : 0;
        }

        void FixedUpdate() {
            float x = transform.position.x;
            // Mathf.Sin - если этой функции подавать значения от 0, она будет генерировать волну
            float y = Mathf.Sin(Time.realtimeSinceStartup * verticalSpeed + randomDeflection) * amplitude;
            float z = transform.position.z;
            transform.localPosition = new Vector3(x, y, z);
        }
    }
}
