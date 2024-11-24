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
        'with_cppcheck': [True, False],
        'with_hdf5': [None, 'shared', 'static'],
        'with_zlib': [None, 'shared', 'static'],
    }
    default_options = {
        'shared': True,
        'extended_sparse': True,
        'fPIC': True,
        'mat73': True,
        'with_cppcheck': False,
        'with_hdf5': 'shared',
        'with_zlib': 'shared',
    }

    def build_requirements(self):
        if self.options.with_cppcheck:
            self.tool_requires('cppcheck/[>=2.16.0]')

    def requirements(self):
        if self.options.with_hdf5:
            self.requires('hdf5/[>=1.8 <1.15]')
        if self.options.with_zlib:
            self.requires('zlib/[>=1.2.3]')
