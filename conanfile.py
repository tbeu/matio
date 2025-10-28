from conan import ConanFile

class MatioConan(ConanFile):
    name = 'matio'
    version = '1.5.28'
    license = 'BSD-2-Clause License'
    description = 'Matio is a C library for reading and writing binary MATLAB MAT files.'
    settings = 'os', 'compiler', 'build_type', 'arch'
    generators = 'CMakeDeps'
    options = {
        'shared': [True, False],
        'extended_sparse': [True, False],
        'fPIC': [True, False],
        'mat73': [True, False],
        'with_hdf5': [None, 'shared', 'static'],
        'with_zlib': [None, 'shared', 'static'],
    }
    default_options = {
        'shared': True,
        'extended_sparse': True,
        'fPIC': True,
        'mat73': True,
        'with_hdf5': 'shared',
        'with_zlib': 'shared',
    }

    def requirements(self):
        if self.options.with_hdf5 and 'Off' != self.conf.get('user.workaround.matio:with_hdf5', default='On'):
            self.requires('hdf5/[>=1.8 <1.15]')
        if self.options.with_zlib and 'Off' != self.conf.get('user.workaround.matio:with_zlib', default='On'):
            self.requires('zlib/[>=1.2.3]')
