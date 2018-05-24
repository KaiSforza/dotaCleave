def stack(String command, String resolver = 'nightly') {
    sh "stack --allow-different-user --resolver ${resolver} ${command}"
}

def resolvers = ['nightly', 'lts-11.10', 'lts-9.21']

// def setupStepParallel = resolvers.collectEntries {
//     ["setup-${it}" : setupStep(it)]
// }
//
// def setupStep(r) {
//     return {
//         node {
//             stack("setup", "${r}")
//         }
//     }
// }
//
// def setupStepExec = resolvers.collectEntries {
//     ["exec-${it}" : execStep(it)]
// }
//
// def execStep(r) {
//     return {
//         node {
//             stack("setup", "${r}")
//         }
//     }
// }


pipeline {
    agent { docker { image 'fpco/stack-build'
                     args "-u root"
    } }
    stages {
        stage("printing") {
            steps {
                echo env.BUILD_ID
                echo env.CHANGE_ID
            }
        }
        stage("setup") {
            parallel {
                stage("setup-${resolvers[0]}") {
                    steps {
                        stack("setup", "${resolvers[0]}")
                    }
                }
                stage("setup-${resolvers[1]}") {
                    steps {
                        stack("setup", "${resolvers[1]}")
                    }
                }
                stage("setup-${resolvers[2]}") {
                    steps {
                        stack("setup", "${resolvers[2]}")
                    }
                }
            }
        }
        stage("build") {
            parallel {
                stage("build-${resolvers[0]}") {
                    steps {
                        stack("build", "${resolvers[0]}")
                    }
                }
                stage("build-${resolvers[1]}") {
                    steps {
                        stack("build", "${resolvers[1]}")
                    }
                }
                stage("build-${resolvers[2]}") {
                    steps {
                        stack("build", "${resolvers[2]}")
                    }
                }
            }
        }
        stage("exec") {
            parallel {
                stage("exec-${resolvers[0]}") {
                    steps {
                        stack("exec", "${resolvers[0]}")
                    }
                }
                stage("exec-${resolvers[1]}") {
                    steps {
                        stack("exec", "${resolvers[1]}")
                    }
                }
                stage("exec-${resolvers[2]}") {
                    steps {
                        stack("exec", "${resolvers[2]}")
                    }
                }
            }
        }
    }
}

            // steps {
            //     script {
            //         stage("bootstraps") {
            //             parallel setupStepParallel
            //         }
            //         // stage("builds") {
            //         //     for (r in resolvers) {
            //         //         stack("build", "${r}")
            //         //     }
            //         // }
            //         stage("run") {
            //             parallel setupStepExec
            //         }
            //     }
            // }
//         }
//     }
// }
