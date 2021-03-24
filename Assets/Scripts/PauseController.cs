//
//  Copyright (c) 2016  FederationOfCoders.org
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
    public class PauseController : MonoBehaviour {
        public Transform mainCamera;
        public int mainCameraLiftSteps;
        public Vector3 mainCameraDestPosition;
        Vector3 mainCameraInitPosition;

        void Start() {
            mainCameraInitPosition = mainCamera.position;
        }

        public void PauseGame() {
            StartCoroutine(mainCamera.TranslateAnimation(mainCameraLiftSteps, mainCameraDestPosition));
            PlayerMover.Instance.enabled = false;
        }

        public void ResumeGame() {
            StartCoroutine(mainCamera.TranslateAnimation(mainCameraLiftSteps, mainCameraInitPosition));
            PlayerMover.Instance.enabled = true;
        }
    }
}
