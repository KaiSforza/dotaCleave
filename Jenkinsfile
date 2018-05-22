pipeline {
    agent { label 'haskell' }
    stages {
        stage('setup-ghcs') {
            steps {
                sh 'stack setup'
                sh 'stack --resolver nightly setup'
            }
        }
        stage('parallel') {
            parallel {
                stage('syntax') {
                    steps {
                        sh 'stack exec -- hlint --git'
                    }
                }
                stage('build') {
                    steps {
                        sh 'stack build'
                    }
                }
                stage('run') {
                    steps {
                        sh 'stack exec -- dotaCleave'
                    }
                }
                stage('build-nightly') {
                    steps {
                        sh 'stack --resolver nightly build'
                    }
                }
                stage('run-nightly') {
                    steps {
                        sh 'stack exec --resolver nightly -- dotaCleave'
                    }
                }
            }
        }
    }
}
