import os
import builtins
import contextlib
import datetime
import hashlib
import importlib
import resource

from glob import glob

from typing import List, Any, Union, Optional, Tuple, Dict, Iterable
from types import ModuleType

import settings


def flatten(nested_list: List[List[Any]]) -> List[Any]:
    return [element for sub_list in nested_list for element in sub_list]


def get_all_files(directory: str, file_regexp: str = "*") -> List[str]:

    if os.path.exists(directory) is False:
        raise Exception(f"Directory dont exists {directory}")

    else:
        all_files = [
            base_path
            for filepath in os.walk(directory)
            for base_path in glob(os.path.join(filepath[0], file_regexp))
            if os.path.isfile(base_path)
        ]

        return all_files


def get_row_count(body: str, newline_delimiter: str) -> int:
    return len([row for row in body.split(newline_delimiter) if len(row) > 0])


def get_chunk(items: List[Dict[str, Any]], chunk_size: int = 50) -> Iterable:
    for index in range(0, len(items), chunk_size):
        yield (items[index : (index + chunk_size)])


def get_chunk_list(items: List[Any], cpu_count: int) -> List[List[Any]]:
    length_dataset = len(items)
    sample_size, _ = divmod(length_dataset, cpu_count)

    index_range = [i * sample_size for i in range(cpu_count)]
    index_range.append(length_dataset)

    return [
        items[index_range[index - 1] : (index_range[index])]
        for index in range(1, len(index_range))
    ]


def get_memory_usage():
    return int(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024)

