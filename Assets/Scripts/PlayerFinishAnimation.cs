using System.Collections;
using UnityEngine;

namespace GridStepper {
    public class PlayerFinishAnimation : MonoBehaviour {
        public float secondsBeforeAnimation;

        public Transform finishCubeGate;
        public Transform moverTransform;

        public int landingSteps;
        public float secondsBeforeClosingGate;
        public int closingGateSteps;

        void Start() {
            StartCoroutine(DoAnimation());
        }

        IEnumerator DoAnimation() {
            yield return new WaitForSeconds(secondsBeforeAnimation);

            yield return StartCoroutine(moverTransform.TranslateAnimation(landingSteps, moverTransform.position + Vector3.down));
            GetComponent<MeshRenderer>().enabled = false;

            yield return new WaitForSeconds(secondsBeforeClosingGate);

            yield return StartCoroutine(finishCubeGate.ScaleAnimation(closingGateSteps, new Vector3(0, 0, 1)));
        }
    }
}