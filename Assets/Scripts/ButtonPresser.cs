using UnityEngine;
using UnityEngine.Events;

namespace GridStepper {
    public class ButtonPresser : MonoBehaviour {
        public Vector3 pressingChange;
        public UnityEvent onPress;
        public UnityEvent onRelease;
        bool isPressed;

        void Press() {
            if (!isPressed) {
                isPressed = true;
                transform.position += pressingChange;
                onPress.Invoke();
            }
        }

        void Release() {
            if (isPressed) {
                isPressed = false;
                transform.position -= pressingChange;
                onRelease.Invoke();
            }
        }

        public void OnMouseDown() {
            Press();
        }

        public void OnMouseUp() {
            Release();
        }
    }
}
