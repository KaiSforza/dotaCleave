pipeline {
    agent { label 'haskell' }
    stages {
        stage('setup-ghcs') {
            steps {
                sh 'stack --stack-root $PWD/stack setup'
                sh 'stack --stack-root $PWD/stack --resolver nightly setup'
            }
        }
        stage('parallel') {
            parallel {
                stage('syntax') {
                    steps {
                        sh 'stack --stack-root $PWD/stack exec -- hlint --git'
                    }
                }
                stage('build') {
                    steps {
                        sh 'stack --stack-root $PWD/stack build'
                    }
                }
                stage('run') {
                    steps {
                        sh 'stack --stack-root $PWD/stack exec -- dotaCleave'
                    }
                }
                stage('build-nightly') {
                    steps {
                        sh 'stack --stack-root $PWD/stack --resolver nightly build'
                    }
                }
                stage('run-nightly') {
                    steps {
                        sh 'stack --stack-root $PWD/stack exec --resolver nightly -- dotaCleave'
                    }
                }
            }
        }
    }
}
