pipeline {
  agent any
  stages {

    stage('Prepare') {
      steps {
        sh 'mkdir -p arivi-core'
        dir(path: 'arivi-core') {
          git(url: 'https://github.com/xoken/arivi-core/', branch: 'master')
        }

        sh 'mkdir -p xoken-core'
        dir(path: 'xoken-core') {
          git(url: 'https://github.com/xoken/xoken-core/', branch: 'master')
        }

        sh 'mkdir -p allegory-reseller'
        dir(path: 'allegory-reseller') {
          git(url: 'https://github.com/xoken/allegory-reseller/', branch: "${env.BRANCH_NAME}")
        }

      }
    }

    stage('Clean') {
      steps {
        dir(path: 'allegory-reseller') {
          sh 'stack clean'
        }

      }
    }

    stage('Build') {
      steps {
        dir(path: 'allegory-reseller') {
          sh 'stack install  --local-bin-path  ../build/reg/'
        }

        archiveArtifacts(artifacts: 'build/**/reseller', followSymlinks: true)
      }
    }



      stage('Release') {
        

        steps {
          script {
            if ((env.BRANCH_NAME).startsWith("release")) {   
              echo '****** Starting Ubuntu18.04 container ******'
              dir(path: 'allegory-reseller'){
                      sh 'rm -f /tmp/reseller-ubuntu1804.cid'
                      sh 'docker run -t -d --cidfile /tmp/reseller-ubuntu1804.cid -w  /opt/work/allegory-reseller  xoken-nexa/ubuntu18.04 sh'
                      sh 'docker exec -w /opt/work/arivi-core $(cat /tmp/reseller-ubuntu1804.cid) git pull'
                      sh 'docker exec -w /opt/work/xoken-core $(cat /tmp/reseller-ubuntu1804.cid) git pull'
                      sh 'docker exec -w /opt/work/ $(cat /tmp/reseller-ubuntu1804.cid) git clone https://github.com/xoken/allegory-reseller.git '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu1804.cid) git fetch '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu1804.cid) git checkout $(basename $(git symbolic-ref HEAD))'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu1804.cid) git pull'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu1804.cid) stack clean'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu1804.cid) stack install  --local-bin-path  app '
                      sh 'docker cp $(cat /tmp/reseller-ubuntu1804.cid):/opt/work/allegory-reseller/app/reseller  ./allegory_reseller '
                      sh 'rm -f /tmp/reseller-ubuntu1804.cid'
                      sh 'sha256sum ./reseller > Checksum_SHA256'
                      sh 'zip allegory-reseller_"$(basename $(git symbolic-ref HEAD))"_ubuntu1804.zip allegory_reseller node-config.yaml README.md Checksum_SHA256 LICENSE LICENSE-AGPL LICENSE-OpenBSV '
                    }
              echo '****** Starting Ubuntu20.04 container ******'
              dir(path: 'allegory-reseller'){
                      sh 'rm -f /tmp/reseller-ubuntu2004.cid'
                      sh 'docker run -t -d --cidfile /tmp/reseller-ubuntu2004.cid -w  /opt/work/allegory-reseller  xoken-nexa/ubuntu20.04 sh'
                      sh 'docker exec -w /opt/work/arivi-core $(cat /tmp/reseller-ubuntu2004.cid) git pull'
                      sh 'docker exec -w /opt/work/xoken-core $(cat /tmp/reseller-ubuntu2004.cid) git pull'
                      sh 'docker exec -w /opt/work/ $(cat /tmp/reseller-ubuntu2004.cid) git clone https://github.com/xoken/allegory-reseller.git '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu2004.cid) git fetch '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu2004.cid) git checkout $(basename $(git symbolic-ref HEAD))'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu2004.cid) git pull'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu2004.cid) stack clean'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-ubuntu2004.cid) stack install  --local-bin-path  app '
                      sh 'docker cp $(cat /tmp/reseller-ubuntu2004.cid):/opt/work/allegory-reseller/app/reseller  ./allegory_reseller '
                      sh 'rm -f /tmp/reseller-ubuntu2004.cid'
                      sh 'sha256sum ./reseller > Checksum_SHA256'
                      sh 'zip allegory-reseller_"$(basename $(git symbolic-ref HEAD))"_ubuntu2004.zip allegory_reseller node-config.yaml README.md Checksum_SHA256 LICENSE LICENSE-AGPL LICENSE-OpenBSV '
                    }
              echo '****** Starting Arch Linux container ******'
              dir(path: 'allegory-reseller'){
                      sh 'rm -f /tmp/reseller-archlinux.cid'
                      sh 'docker run -t -d --cidfile /tmp/reseller-archlinux.cid -w  /opt/work/allegory-reseller  xoken-nexa/archlinux sh'
                      sh 'docker exec -w /opt/work/arivi-core $(cat /tmp/reseller-archlinux.cid) git pull'
                      sh 'docker exec -w /opt/work/xoken-core $(cat /tmp/reseller-archlinux.cid) git pull'
                      sh 'docker exec -w /opt/work/ $(cat /tmp/reseller-archlinux.cid) git clone https://github.com/xoken/allegory-reseller.git '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-archlinux.cid) git fetch '
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-archlinux.cid) git checkout $(basename $(git symbolic-ref HEAD))'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-archlinux.cid) git pull'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-archlinux.cid) stack clean'
                      sh 'docker exec -w /opt/work/allegory-reseller $(cat /tmp/reseller-archlinux.cid) env LD_PRELOAD=/usr/lib/libjemalloc.so.2 stack install  --local-bin-path  app '
                      sh 'docker cp $(cat /tmp/reseller-archlinux.cid):/opt/work/allegory-reseller/app/reseller ./allegory_reseller '
                      sh 'rm -f /tmp/reseller-archlinux.cid'
                      sh 'sha256sum ./reseller > Checksum_SHA256'
                      sh 'zip allegory-reseller_"$(basename $(git symbolic-ref HEAD))"_archlinux.zip allegory_reseller node-config.yaml README.md Checksum_SHA256 LICENSE LICENSE-AGPL LICENSE-OpenBSV '
                    }              
                    archiveArtifacts(artifacts: 'allegory-reseller/allegory-reseller*.zip', followSymlinks: true)
          } else { 
          echo 'skipping Docker release packaging..'
          }
        }
        } 
        
       } 

    } 
    
  
      post {
          unsuccessful {
                  emailext(subject: '$PROJECT_NAME - Build # $BUILD_NUMBER - $BUILD_STATUS!', body: '$PROJECT_NAME - Build # $BUILD_NUMBER - $BUILD_STATUS    ||   Please check attached logfile for more details.', attachLog: true, from: 'buildmaster@xoken.org', replyTo: 'buildmaster@xoken.org', to: 'jenkins-notifications@xoken.org')
           
          }
          fixed {
                  emailext(subject: '$PROJECT_NAME - Build # $BUILD_NUMBER - $BUILD_STATUS!', body: '$PROJECT_NAME - Build # $BUILD_NUMBER - $BUILD_STATUS  ||  Previous build was not successful and the current builds status is SUCCESS ||  Please check attached logfile for more details.', attachLog: true, from: 'buildmaster@xoken.org', replyTo: 'buildmaster@xoken.org', to: 'jenkins-notifications@xoken.org')
          }
      }
  
  
}
