from fabric.api import task
from fabric.api import local

@task
def apns_cert(cer_file, key_file, pem_file):
    '''
    Generate PEM file which combines certificate and private key. Usually, you would like to copy this PEM file to 
    your web server.
    
    To use this script, you should download the certificate from Apple developer center first. Double-click the file
    to install the certificate. Then in Keychain Assist, you should be able to expand the certificate and download
    private key (.p12 format)

    Args:
        cer_file: Input certificate file (.cer)
        key_file: Private key file (.p12)
        pem_file: Output PEM certificate file
    '''
    local("openssl x509 -in %s -inform der -out app_cert.pem" % cer_file)
    local("openssl pkcs12 -in %s -out app_key.pem  -nodes" % key_file)
    local("cat app_cert.pem app_key.pem > %s" % pem_file)
    local("openssl s_client -connect gateway.push.apple.com:2195 -cert %s -key %s" % (pem_file, pem_file))
