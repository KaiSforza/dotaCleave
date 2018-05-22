pipeline {
    agent { label 'haskell' }
    stages {
        stage('setup-ghcs') {
            steps {
                sh 'stack --stack-root $PWD/stack setup'
                sh 'stack --stack-root $PWD/stack --resolver nightly setup'
            }
        }
        stage('build-stable') {
            steps {
                sh 'stack --stack-root $PWD/stack build'
            }
        }
        stage('build-nightly') {
            steps {
                sh 'stack --stack-root $PWD/stack --resolver nightly build'
            }
        }
        stage('runs') {
            parallel {
                stage('syntax') {
                    steps {
                        sh 'ls -la'
                        sh 'stack --stack-root $PWD/stack exec -- hlint --git dotaCleave/'
                    }
                }
                stage('run-stable') {
                    steps {
                        sh 'stack --stack-root $PWD/stack exec -- dotaCleave'
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
