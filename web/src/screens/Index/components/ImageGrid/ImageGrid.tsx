import Image               from '../Image';
import Pager               from '../Pager';
import React, { useState } from 'react';
import styles              from './ImageGrid.module.css';
import { Link }            from 'react-router-dom';
import { image }           from '../../../../types/image';

interface Props {
    images: image[];
    width: number;
    rows: number;
}

// Render images that have actually got valid paths.
const renderValidImages = (slice: image[], props: Props) =>
    slice.map((image, k) =>
        <Link key={image.path} to="/i/8ac5928b-9caa3ac1-cb488a9a-938ac938">
            <Image
                path={image.path}
                width={960 / props.width}
                resolution="800x600"
            />
        </Link>
    );


// Render empty spaces for all images that are missing to keep the grid size
// fixed at all times.
const renderEmptyImages = (slice: image[], props: Props) =>
    Array(props.width * props.rows - slice.length)
        .fill(0)
        .map(k =>
            <Image key={k.toString()} width={960 / props.width} empty />
        );


// Render a fixed grid, calculating which images to display depending on page
// width and row count.
const ImageGrid = (props: Props) => {
    // Render State
    const [page, setPage] = useState(1);
    const imageView       = props.images.slice(
        props.width * props.rows * (page - 1 + 0),
        props.width * props.rows * (page - 1 + 1)
    );

    return (
        <div className={styles.ImageGrid}>
            { renderValidImages(imageView, props) }
            { renderEmptyImages(imageView, props) }

            <Pager
                page={page}
                pageCount={5}
                setPage={(n: number) => setPage(Math.max(1, n))}
            />
        </div>
    );
}

export default ImageGrid;
