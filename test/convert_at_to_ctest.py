import glob
import hashlib
import json
import os
import re
import sys

condition_mapping = {
    'test $EXTENDED_SPARSE -ne 1': 'MATIO_EXTENDED_SPARSE',
    'test $COMPRESSION_ZLIB -ne 1': 'HAVE_ZLIB',
    'test $MAT73 -ne 1': 'MAT73 AND HAVE_HDF5',
    'test $HAVE_INT64 -eq 0': 'HAVE_INT64',
    'test $HAVE_UINT64 -eq 0': 'HAVE_UINT64',
    '! grep -q "#define SIZEOF_VOID_P 4" $abs_top_builddir/src/matioConfig.h': 'CMAKE_SIZEOF_VOID_P EQUAL 4',
    '! grep -q "#define SIZEOF_VOID_P 8" $abs_top_builddir/src/matioConfig.h': 'CMAKE_SIZEOF_VOID_P EQUAL 8',
}

command_mapping = {
    '$srcdir/': '${PROJECT_SOURCE_DIR}/test/',
    '$builddir/test_mat': '$<TARGET_FILE:test_mat>',
    '$builddir/../tools/matdump': '$<TARGET_FILE:matdump>',
}


def convert_autotest_to_ctest(autotest_file, cmake_output_file):
    base_name = os.path.splitext(os.path.basename(autotest_file))[0]

    with open(autotest_file, 'r') as atf, open(cmake_output_file, 'w') as cmakef:
        cmakef.write('# Auto-generated CMake/CTest tests\n')

        setup_re = re.compile(r'AT_SETUP\(\[([^\]]+)\]\)')
        skip_if_re = re.compile(r'AT_SKIP_IF\(\[([^\]]+)\]\)')
        check_re = re.compile(r'AT_CHECK\(\[([^\]]+)\]')
        check_copy_re = re.compile(r'AT_CHECK\(\[cp (.*?) expout')
        check_diff_re = re.compile(r'\s*(.*?)\],\[0\],\[expout\],\[\]\)')
        cleanup_re = re.compile(r'AT_CLEANUP')
        keyword_re = re.compile(r'AT_KEYWORDS\(\[([^\]]+)\]\)')

        skip_conditions = []
        test_keywords = []
        counter = 1
        check_copy_match = None
        check_diff_match = None

        for line in atf:
            setup_match = setup_re.search(line)
            skip_if_match = skip_if_re.search(line)
            check_match = check_re.search(line)
            if check_copy_match is None:
                check_copy_match = check_copy_re.search(line)
            if check_diff_match is None:
                check_diff_match = check_diff_re.search(line)
            cleanup_match = cleanup_re.search(line)
            keyword_match = keyword_re.search(line)

            if setup_match:
                test_name = f'{base_name}_{re.sub(r"[ ()]", "_", setup_match.group(1))}'.lower()
                cmakef.write(f'\n# {setup_match.group(1)}\n')
                skip_conditions = ['True']
                test_keywords = [base_name]
                test_keywords.extend(base_name.split('_'))
                counter = 1
                check_copy_match = None
                check_diff_match = None

            elif keyword_match:
                keywords = keyword_match.group(1).strip().split()
                test_keywords.extend(keywords)

            elif skip_if_match:
                condition = skip_if_match.group(1).strip()

                if condition in condition_mapping:
                    mapped_condition = condition_mapping[condition]
                    skip_conditions.append(mapped_condition)
                else:
                    skip_conditions.append(condition)

            elif check_match:
                command = check_match.group(1)
                for original, replacement in command_mapping.items():
                    command = command.replace(original, replacement)

                if skip_conditions:
                    if len(skip_conditions) > 1:
                        skip_conditions = skip_conditions[1:]
                    combined_conditions = ' AND '.join(skip_conditions)
                    cmakef.write(f'if({combined_conditions})\n')
                    skip_conditions = []

                cmakef.write(f'    add_test(NAME {test_name}_{counter}\n')
                cmakef.write(f'        COMMAND {command}\n')
                cmakef.write('        WORKING_DIRECTORY ${MATIO_TESTING_DIR})\n')
                cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES FIXTURES_REQUIRED TEMPDIR)\n')

                if test_keywords:
                    keyword_str = ';'.join(sorted(list(set(test_keywords))))
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES LABELS "{keyword_str}")\n')

                if counter > 1:
                    depends_str = f'{test_name}_{counter - 1}'
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES DEPENDS {depends_str})\n')

                if 'write' in base_name or 'delete' in test_keywords:
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES RUN_SERIAL ON)\n')

                counter += 1

            elif check_copy_match and check_diff_match:
                command = check_diff_match.group(1)
                for original, replacement in command_mapping.items():
                    command = command.replace(original, replacement)

                output_name = f'{test_name}_{counter}_output.txt'
                if command.startswith('$<TARGET_FILE:'):
                    command += f' -o {output_name}'

                if skip_conditions:
                    if len(skip_conditions) > 1:
                        skip_conditions = skip_conditions[1:]
                    combined_conditions = ' AND '.join(skip_conditions)
                    cmakef.write(f'if({combined_conditions})\n')
                    skip_conditions = []

                cmakef.write(f'    add_test(NAME {test_name}_{counter}\n')
                cmakef.write(f'        COMMAND {command}\n')
                cmakef.write('        WORKING_DIRECTORY ${MATIO_TESTING_DIR})\n')
                cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES FIXTURES_REQUIRED TEMPDIR)\n')

                if test_keywords:
                    keyword_str = ';'.join(sorted(list(set(test_keywords))))
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES LABELS "{keyword_str}")\n')

                if counter > 1:
                    depends_str = f'{test_name}_{counter - 1}'
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES DEPENDS {depends_str})\n')

                if 'write' in base_name or 'delete' in test_keywords:
                    cmakef.write(f'    set_tests_properties({test_name}_{counter} PROPERTIES RUN_SERIAL ON)\n')

                expected_name = check_copy_match.group(1).replace('$srcdir/', '${PROJECT_SOURCE_DIR}/test/')
                command = f'${{CMAKE_COMMAND}} -E compare_files --ignore-eol {expected_name} {output_name}'
                cmakef.write(f'    add_test(NAME {test_name}_{counter + 1}\n')
                cmakef.write(f'        COMMAND {command}\n')
                cmakef.write('        WORKING_DIRECTORY ${MATIO_TESTING_DIR})\n')
                cmakef.write(f'    set_tests_properties({test_name}_{counter + 1} PROPERTIES FIXTURES_REQUIRED TEMPDIR)\n')

                if test_keywords:
                    keyword_str = ';'.join(sorted(list(set(test_keywords))))
                    cmakef.write(f'    set_tests_properties({test_name}_{counter + 1} PROPERTIES LABELS "diff;{keyword_str}")\n')

                depends_str = f'{test_name}_{counter}'
                cmakef.write(f'    set_tests_properties({test_name}_{counter + 1} PROPERTIES DEPENDS {depends_str})\n')

                check_copy_match = None
                check_diff_match = None
                counter += 2

            elif cleanup_match:
                if counter > 1:
                    cmakef.write('endif()\n')


def get_file_hash(file_path, algorithm):
    hash_obj = hashlib.new(algorithm)
    with open(file_path, 'rb') as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_obj.update(chunk)
    return hash_obj.hexdigest()


def generate_ctest_files(file_list, output_dir, force):
    os.makedirs(output_dir, exist_ok=True)

    for autotest_file in file_list:
        base_name = os.path.splitext(os.path.basename(autotest_file))[0]
        cmake_output_file = os.path.join(output_dir, f'{base_name}.cmake')
        if not force and os.path.isfile(cmake_output_file):
            continue
        convert_autotest_to_ctest(autotest_file, cmake_output_file)


if __name__ == '__main__':
    algorithm = 'sha256'
    new_hash = get_file_hash(__file__, algorithm)
    output_dir = sys.argv[1]
    hash_json_file = os.path.join(output_dir, 'generator.json')
    try:
        with open(hash_json_file, 'r') as json_file:
            old_hash = json.load(json_file).get(algorithm, '');
    except OSError:
        old_hash = ''
    force = new_hash != old_hash

    generate_ctest_files(glob.glob(os.path.join('tests', '*.at')), output_dir, force)

    with open(hash_json_file, 'w') as json_file:
        json.dump({algorithm: new_hash}, json_file)
