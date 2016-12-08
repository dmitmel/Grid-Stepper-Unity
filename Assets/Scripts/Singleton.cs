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
    public class Singleton<T> : MonoBehaviour where T : MonoBehaviour {
        private static T _instance;

        public void Awake() {
            // Если в сцене уже есть объект с таким компонентом, то
            // он пропишет себя в _instance при инициализации
            if (!_instance) {
                _instance = gameObject.GetComponent<T>();
            } else {
                Debug.LogError("[Singleton] Создан второй экземпляр '" + typeof(T) + "'!");
            }
        }

        public static T Instance {
            get {
                if (_instance == null) {
                    _instance = (T) FindObjectOfType(typeof(T));

                    if (FindObjectsOfType(typeof(T)).Length > 1) {
                        Debug.LogError("[Singleton] Найдено несколько экземпляров '" + typeof(T) + "'!");
                    }

                    if (_instance == null) {
                        // Если в сцене объектов с этим классом нет - создаём новый GameObject и лепим ему наш компонент
                        GameObject singleton = new GameObject();
                        _instance = singleton.AddComponent<T>();
                        singleton.name = "(singleton) " + typeof(T).ToString();
                        DontDestroyOnLoad(singleton);
                        Debug.Log("[Singleton] Был создан экземпляр '" + typeof(T) + "': " + singleton);
                    } else {
                        Debug.Log("[Singleton] Используется экземпляр '" + typeof(T) + "': " + _instance.gameObject.name);
                    }
                }

                return _instance;
            }
        }
    }
}