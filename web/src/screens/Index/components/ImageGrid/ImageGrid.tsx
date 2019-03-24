import React           from 'react';
import { Link }        from 'react-router-dom';
import { image }       from '../../../../types/image';
import { ScalingMode } from '../../../../store/images/types';

import Image     from '../Image';
import Pager     from '../Pager';
import styles    from './ImageGrid.module.css';


interface Props {
    changePage:     (page: number) => void;
    scalingMode:    ScalingMode;
    setScalingMode: (mode: ScalingMode) => void;
    images:         image[];
    width:          number;
    rows:           number;
    page:           number;
}

// Render images that have actually got valid paths.
const renderValidImages = (slice: image[], props: Props) =>
    slice.map((image, k) =>
        <Link key={image.path} to="/i/8ac5928b-9caa3ac1-cb488a9a-938ac938">
            <Image
                contain={props.scalingMode === 'contain'}
                key={k.toString()}
                path={image.path}
                resolution="800x600"
                width={944 / props.width}
            />
        </Link>
    );


// Render empty spaces for all images that are missing to keep the grid size
// fixed at all times.
const renderEmptyImages = (slice: image[], props: Props) =>
    Array(props.width * props.rows - slice.length)
        .fill(0)
        .map(k =>
            <Image
                empty
                contain={props.scalingMode === 'contain'}
                key={k.toString()}
                width={944 / props.width}
            />
        );


// Render a fixed grid, calculating which images to display depending on page
// width and row count.
const ImageGrid = (props: Props) => {
    const imageView = props.images.slice(
        props.width * props.rows * (props.page - 1 + 0),
        props.width * props.rows * (props.page - 1 + 1)
    );

    if (imageView.length === 0 ) {
        return (
            <div>
                Nodbody here but us chickens!
            </div>
        );
    }

    return (
        <div className={styles.ImageGrid}>
            { renderValidImages(imageView, props) }
            { renderEmptyImages(imageView, props) }

            <Pager
                page={props.page}
                pageCount={5}
                scalingMode={props.scalingMode}
                setPage={props.changePage}
                setScalingMode={props.setScalingMode}
            />
        </div>
    );
}

export default ImageGrid;
