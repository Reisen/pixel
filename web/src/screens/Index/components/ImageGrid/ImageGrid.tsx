import React           from 'react';
import Cookies         from 'universal-cookie';
import { Link }        from 'react-router-dom';
import { image }       from '../../../../types/image';
import { ScalingMode } from '../../../../store/images/types';

import Image           from '../Image';
import styles          from './ImageGrid.module.css';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('base') || 'http://localhost:3000';
};

interface Props {
    changePage:     (page: number) => void;
    scalingMode:    ScalingMode;
    setScalingMode: (mode: ScalingMode) => void;
    images:         image[];
    cols:           number;
    rows:           number;
}

// Render images that have actually got valid paths.
const renderValidImages = (slice: image[], props: Props) =>
    slice.map((image, k) =>
        <Image
            contain={props.scalingMode === 'contain'}
            path={`${image.path}`}
            resolution="800x600"
            width={944 / props.cols}
        />
    );


// Render empty spaces for all images that are missing to keep the grid size
// fixed at all times.
const renderEmptyImages = (slice: image[], props: Props) =>
    Array(props.cols * props.rows - slice.length)
        .fill(0)
        .map((k, v) =>
            <Image
                empty
                contain={props.scalingMode === 'contain'}
                key={v.toString()}
                width={944 / props.cols}
            />
        );


// Render a fixed grid, calculating which images to display depending on page
// width and row count.
const ImageGrid = (props: Props) => {
    if (props.images.length === 0 ) {
        return (
            <div className={styles.OhNo}>
                <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Chicken_cartoon_04.svg/723px-Chicken_cartoon_04.svg.png" alt="Chickens" /><br/>
                Nobody here but us chickens!
            </div>
        );
    }

    return (
        <div className={styles.ImageGrid}>
            { renderValidImages(props.images, props) }
            { renderEmptyImages(props.images, props) }
        </div>
    );
}

export default ImageGrid;
