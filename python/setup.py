#!/usr/bin/env python
"""Setup script for hearthstone-nash package."""
from setuptools import setup, find_packages

setup(
    name="hearthstone-nash",
    version="0.1.0",
    description="Nash equilibrium calculator for Hearthstone tournament formats",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    author="Hearthstone Nash Contributors",
    license="MIT",
    python_requires=">=3.8",
    packages=find_packages(),
    install_requires=[
        "numpy>=1.20.0",
        "scipy>=1.7.0",
    ],
    extras_require={
        "dev": ["pytest>=7.0.0"],
    },
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
)
