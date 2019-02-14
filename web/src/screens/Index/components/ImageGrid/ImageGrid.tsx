import Image     from '../Image';
import Pager     from '../Pager';
import React     from 'react';
import styles    from './ImageGrid.module.css';
import { Link }  from 'react-router-dom';
import { image } from '../../../../types/image';

interface Props {
    images: image[];
    width: number;
}

const renderValidImages = (images: image[]) => images.map(image =>
    <Link to="/i/8ac5928b-9caa3ac1-cb488a9a-938ac938">
        <Image resolution="800x600" />
    </Link>
);

const renderEmptyImages = (total: number, width: number) => {
    const emptyCount = width - (total % width);
    const fillSlice  = Array(emptyCount).fill(0);
    return fillSlice.map(x =>
        <Image empty />
    );
}

const ImageGrid = (props: Props) => (
    <div className={styles.ImageGrid}>
        { renderValidImages(props.images) }
        { renderEmptyImages(props.images.length, props.width) }

        <Pager
          current={1}
          total={Math.floor(props.images.length / 24)}
        />
    </div>
);

export default ImageGrid;
