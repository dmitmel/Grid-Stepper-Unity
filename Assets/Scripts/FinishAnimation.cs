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

namespace GridStepper {
    [Serializable]
    public class LandInFinishCubeStep {
        public float secondsBeforeStart;
        public float landingStep;
    }

    [Serializable]
    public class CloseFinishCubeGateStep {
        public float secondsBeforeStart;
        public GameObject finishCubeGate;
        public float closingStep;
    }

    public class FinishAnimation : MonoBehaviour {
        public float secondsBeforeAnimation;

        public LandInFinishCubeStep landInFinishCubeStep;
        public CloseFinishCubeGateStep closeFinishCubeGateStep;

        void Start() {
            StartCoroutine(PlayAnimation());
        }

        private IEnumerator PlayAnimation() {
            yield return StartCoroutine(PrepareAnimation());
            yield return StartCoroutine(LandInFinishCube());
            yield return StartCoroutine(CloseFinishCubeGate());
        }

        private IEnumerator PrepareAnimation() {
            GetComponent<PlayerController>().enabled = false;
            yield return new WaitForSeconds(secondsBeforeAnimation);
            GetComponent<HoverEffect>().enabled = false;
        }

        private IEnumerator LandInFinishCube() {
            yield return new WaitForSeconds(landInFinishCubeStep.secondsBeforeStart);

            float landingStep = landInFinishCubeStep.landingStep;
            for (float y = transform.position.y; y > 0; y -= landingStep) {
                transform.Translate(0, -landingStep, 0);
                yield return null;
            }

            transform.position = new Vector3(transform.position.x, 0, transform.position.z);
        }

        private IEnumerator CloseFinishCubeGate() {
            yield return new WaitForSeconds(landInFinishCubeStep.secondsBeforeStart);

            Transform gate = closeFinishCubeGateStep.finishCubeGate.transform;

            float closingStep = closeFinishCubeGateStep.closingStep;
            for (float scale = gate.localScale.x; scale > 0; scale -= closingStep) {
                gate.localScale += new Vector3(-closingStep, -closingStep, 0);
                yield return null;
            }

            gate.localScale = new Vector3(0, 0, gate.localScale.z);
        }
    }
}